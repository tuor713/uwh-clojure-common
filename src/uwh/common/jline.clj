(ns uwh.common.jline
  (:import [jline ArgumentCompletor Completor ConsoleReader FileNameCompletor MultiCompletor NullCompletor SimpleCompletor]
	   [java.io FileDescriptor FileInputStream OutputStreamWriter])
  (:use uwh.common.string))

(defn completor
  "Create a completor: <string> -> completions"
  [f]
  (proxy [Completor] []
    (complete [buffer cursor candidates]
	      (.addAll candidates (f buffer))
	      0)))

(defn typed-completor
  "Create a completor suitable for the type argument"
  [type]
  (cond
   (sequential? type) (SimpleCompletor. (into-array String type))
   (= type :any) (NullCompletor.)
   (= type :file) (FileNameCompletor.)
   :otherwise (completor type)))

(defn command-completor
  "Create a jline.Completor for the command.
<name> - the command name
<types> - a sequence of argument types (:file, list of options, function ...)
<open-ended> - whether the last argument can be repeated arbitrarily often
"
  [name types open-ended]
  (let [name-completor (SimpleCompletor. (into-array String [name]))
	type-completors (map typed-completor types)
	arg-completors (if open-ended
			 type-completors
			 (concat type-completors [(NullCompletor.)]))]
    (ArgumentCompletor. (vec (cons name-completor arg-completors)))))

(defmacro commands
  "Macro for defining a set of commands.
Use like:
(commands
 (echo [arg :file choice [\"one\" \"two\"] & more (constantly [\"option\"])]
	       (apply str arg choice more))
"
  [ & specs]
  `(hash-map
    ~@(mapcat
       (fn [[name args & body]]
	 (let [sep (some #(= % '&) args)
	       types (map second (partition 2 (filter #(not= % '&)
						      args)))
	       arg-names (map first (partition 2 (mapcat #(if (= % '&)
							    ['& :dummy] [%])
							 args)))]
	   `(~(str name)
	     {:function (fn [~@arg-names] ~@body)
	      :completor (command-completor ~(str name)
					    [~@types]
					    ~sep)})))
       specs)))


(declare ^{:doc "Thread local atom to determine when the interactive shell should be terminated"}
	 *exit*)

(defn run
  "Run an interactive shell with the given set of commands and options"
  [cmds & opts]
  (let [{prompt :prompt in :in out :out} (merge {:prompt "> "}
						(apply hash-map opts))
	real-in (or in (FileInputStream. FileDescriptor/in))
	real-out (or out (OutputStreamWriter. System/out))
	reader (ConsoleReader. real-in real-out)]

    (.addCompletor reader (MultiCompletor.
			   (vec (map (comp :completor second) cmds))))

    (binding [*exit* (atom false)
	      *out* real-out]
      (loop []
	(when-not (deref *exit*)
	  (try
	    (let [cmd-line (.readLine reader prompt)
		  [cmd & args] (split-quoted (.trim cmd-line))
		  cmd-obj (cmds cmd)]
	      (if (nil? cmd-obj)
		(println "Unknown command: " cmd)
		(apply (:function cmd-obj) args)))
	    (catch Exception e (.printStackTrace e)))
	  (flush)
	  (recur))))))







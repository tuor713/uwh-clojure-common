(ns uwh.common.jline-test
  (:use clojure.test
	[uwh.common.jline :only [commands completor run *exit*]])
  (:import [java.io ByteArrayInputStream StringWriter]))

(defn complete
  ([c s] (complete c s 0))
  ([c s idx]
     (let [l (java.util.ArrayList.)]
       (.complete c s idx l)
       (vec l))))

(deftest completor-test
  (let [sut (completor (fn [s] (filter #(.startsWith % s)
				       ["hello" "hi" "world"])))]
    (is (= ["hello" "hi"] (complete sut "h")))
    (is (= [] (complete sut "a")))))

(deftest commands-test
  (let [sut (commands
	     (noargs [] "called")
	     (echo [s :any] s)
	     (cat [s :file] s)
	     (mcat [ & fs :file] fs)
	     (dcat [s :file s2 :file] (str s s2))
	     (opts [s ["hello" "world"]] s)
	     (custom [s (fn [in] ["hello"])] s))]

    (let [{f :function comp :completor} (sut "noargs")]
      (is (= "called" (f)))
      (is (= ["noargs "] (complete comp "no"))))

    (let [{f :function comp :completor} (sut "echo")]
      (is (= "input" (f "input")))
      (is (= ["echo "] (complete comp "ec")))
      (is (= [] (complete comp "echo n" 6))))

    (let [{comp :completor} (sut "cat")]
      (is (= ["cat "] (complete comp "c")))
      (is (= ["jline_test.clj "]
	       (complete comp "cat test/uwh/common/jline" 12)))
      (is (= [] (complete comp "cat test/ tes" 12))))

    (let [{comp :completor} (sut "custom")]
      (is (= ["hello"] (complete comp "custom he" 8))))

    (let [{comp :completor} (sut "opts")]
      (is (= ["hello "] (complete comp "opts he" 7))))
    
    (let [{f :function comp :completor} (sut "dcat")]
      (is (= "onetwo" (f "one" "two")))
      (is (= ["test/"] (complete comp "dcat test/ tes" 13))))

    (let [{f :function comp :completor} (sut "mcat")]
      (is (= ["one" "two"] (f "one" "two")))
      (is (= ["test/"] (complete comp "mcat test/ tes" 13))))))


(defmacro test-shell-with-opts
  [input opts & cmds]
  `(let [in# (ByteArrayInputStream. (.getBytes ~input))
	 out# (StringWriter.)]
     (run (commands
	   (~(str "exit") [] (reset! *exit* true))
	   ~@cmds)
	  :in in# :out out# ~@opts)
     (str out#)))

(defmacro test-shell
  "Test a complete interactive shell run, taking an input string including tabs for completion and new lines.
Care needs to be taken to make sure that the command sequence ends in exit to prevent infinite runs."
  [input & cmds]
  `(test-shell-with-opts ~input [] ~@cmds))

(deftest base-test
  (is (= "> exit\n"
	 (test-shell "exit\n"))))

(deftest base-completion
  (is (= "> e\b \bexit \n"
	 (test-shell "e\t\n"))))


(deftest multi-completion
  (is (= "> e\b \bex\n\nexodus    exit\n> exit\n"
	 (test-shell "e\tit\n"
		     (exodus [] (println "wrong command"))))))

(deftest multi-command
  (is (= "> hello\nworld\n> exit\n"
	 (test-shell "hello\nexit\n"
		     (hello [] (println "world"))))))

(deftest file-completion
  (is (= "> echo t\b \btest/\ntest/\n> exit\n"
	 (test-shell "echo t\t\nexit\n"
		     (echo [f :file] (println f))))))

(deftest choice-completion
  (is (= "> hello w\b \bworld \nworld\n> exit\n"
	 (test-shell "hello w\t\nexit\n"
		     (hello [s ["universe" "world"]] (println s))))))

(deftest func-completion
  (is (= "> echo w\b \bworld \nworld\n> exit\n"
	 (test-shell "echo w\t\nexit\n"
		     (echo [s (constantly ["world "])] (println s))))))

(deftest custom-prompt
  (is (= "#exit\n"
	 (test-shell-with-opts "exit\n" [:prompt "#"]))))


(ns uwh.common.string)

(defn- make-filler [s num filler]
  (let [fill-length (max 0 (- num (count s)))
	repeat-count (int (Math/ceil (/ fill-length (count filler))))
	fill-string (.substring (apply str (repeat repeat-count filler)) 0 fill-length)]
    fill-string))

(defn ljust
  ([s num] (ljust s num " "))
  ([s num filler] (str s (make-filler s num filler))))

(defn rjust
  ([s num] (rjust s num " "))
  ([s num filler]
    (str (make-filler s num filler) s)))

(defn fuzzy-match
  "Determine whether two strings are fuzzy matches (by default case-insensitive)"
  ([s target] (fuzzy-match s false target))
  ([s case-sensitive target]
     (let [ss (if case-sensitive s (.toLowerCase s))
	   pattern (re-pattern (apply str ".*"
				      (interleave ss (repeat ".*"))))]
       (re-matches pattern (if case-sensitive target (.toLowerCase target))))))


(defn disambiguate [s opts]
  (reduce
   (fn [res f]
     (if (nil? res)
       (let [reduced (filter f opts)]
	 (if (= (count reduced) 1)
	   (first reduced)
	   nil))
       res))
   nil
   [#(= s %)
    #(.equalsIgnoreCase s %)
    #(.contains % s)
    #(.contains (.toLowerCase %) (.toLowerCase s))
    #(fuzzy-match s false %)
    #(fuzzy-match s true %)]))


(defn- is-separator [c]
  (or (Character/isWhitespace c)
      (= c \")
      (= c \')))

(defn split-quoted
  "Split a line around spaces taking care not to split but to strip quotes or double quotes"
  [s]
  (loop [remainder s parts [] terminus nil]
    (cond
     (empty? remainder)
     parts

     (nil? terminus)
     (let [[part new-rest] (split-with (complement is-separator)
				       (drop-while #(Character/isWhitespace %)
						   remainder))
	   new-parts (if (empty? part)
		       parts
		       (conj parts (apply str part)))]
       (if (or (empty? new-rest) (Character/isWhitespace (first new-rest)))
	 (recur new-rest new-parts nil)
	 (recur (rest new-rest)
		new-parts
		(first new-rest))))

     :otherwise
     (let [[part new-rest] (split-with #(not= terminus %) remainder)]
       (when (or (empty? new-rest)
		 (not= terminus (first new-rest)))
	 (throw (IllegalArgumentException. "Unmatched quotes")))

       (recur (rest new-rest)
	      (conj parts (apply str part))
	      nil)))))


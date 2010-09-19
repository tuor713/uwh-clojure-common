(ns uwh.common.seq)


(defn mmap
  "Create a new map from associating to each key the result of apply f to [<key> <value>]"
  [f h]
  (reduce (fn [res [k v]] (assoc res k (f [k v])))
	  {}
	  h))

(defn map-to-hash
  "Take a sequence and create a hash-map from v -> (f v)"
  [f cols]
  (reduce (fn [res v] (assoc res v (f v)))
	  {}
	  cols))

(defn cross-set
  "Calculates the cross product of the input sets"
  ([s] (map list s))
  ([x & xs]
     (mapcat
      (fn [r] (map #(cons % r)
		   x))
      (apply cross-set xs))))
  
(defn apply*
  "f => (fn [args] (apply f args))"
  [f] (fn [args] (apply f args)))


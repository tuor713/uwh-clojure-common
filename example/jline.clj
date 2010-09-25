(use 'uwh.common.jline)
(use '[clojure.contrib.io :only (file read-lines)])

(def processor (atom identity))
(defn nil-safe [f] #(if (nil? %) % (f %)))
(defn add! [f] (swap! processor #(comp (nil-safe f) %)))

(run
 (commands
  (transform [f :file]
	     (doall (map (comp (nil-safe println) @processor)
			 (read-lines (file f)))))

  (reset [] (reset! processor identity))
  (include [re :any]
	   (add! #(if (.matches % (str ".*" re ".*"))
		    % nil)))
  (exclude [re :any]
	   (add! #(if (.matches % (str ".*" re ".*"))
		    nil %)))
  (replace [re :any s :any]
	   (add! #(.replaceAll % re s)))
  
  (exit [] (reset! *exit* true))))



(ns uwh.common.seq-test
  (:use clojure.test
	uwh.common.seq))

(deftest test-mmap
  (is (= {:a 1 :b 3} (mmap #(inc (% 1)) {:a 0 :b 2}))))

(deftest test-map-to-hash
  (is (= {1 1 2 4} (map-to-hash #(* % %)
				[1 2]))))

(deftest test-cross-set
  (is (= [[1]] (cross-set [1])))
  (is (= [[1 1] [2 1] [1 2] [2 2]] (cross-set [1 2] [1 2])))
  (is (= [[1 2 3] [1 2 4]] (cross-set [1] [2] [3 4]))))

(deftest test-apply*
  (is (= 6 ((apply* +)
	    [1 2 3]))))
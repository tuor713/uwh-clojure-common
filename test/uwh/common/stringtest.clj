(ns uwh.common.stringtest
  (:use clojure.test
	uwh.common.string))

(deftest ljusttest
  (is (= "hello---" (ljust "hello" 8 "-")))
  (is (= "hello   " (ljust "hello" 8)))
  (is (= "hello" (ljust "hello" 2)))
  (is (= "hello" (ljust "hello" 5)))
  (is (= "hello " (ljust "hello" 6)))
  (is (= "hello++" (ljust "hello" 7 "++")))
  (is (= "hello+-+" (ljust "hello" 8 "+-"))))

(deftest rjusttest
  (is (= "world" (rjust "world" 2)))
  (is (= "world" (rjust "world" 5)))
  (is (= "   world" (rjust "world" 8)))
  (is (= "---world" (rjust "world" 8 "-")))
  (is (= "++world" (rjust "world" 7 "++")))
  (is (= "+-+world" (rjust "world" 8 "+-"))))

(deftest disambiguate-test
  (are [res s opts] (= res (disambiguate s opts))
       nil "he" ["hello" "help"]
       "he" "he" ["he" "hello" "help"]
       "help" "he" ["help" "Hello"]
       "help" "ep" ["help" "hello"]
       "epoch" "ep" ["help" "hello" "epoch"]
       nil "hl" ["help" "hello"]))

(deftest test-fuzzy-match
  (is (fuzzy-match "hl" "hello"))
  (is (not (fuzzy-match "hol" "hello")))
  (is (fuzzy-match "hl" "Hello"))
  (is (not (fuzzy-match "hl" true "Hello")))
  (is (fuzzy-match "Hl" true "Hello")))

(deftest split-quote-test
  (are [res s] (= res (split-quoted s))
       [] ""
       ["no-space"] "no-space"
       ["some" "string"] "some string"
       ["some" "quoted string"] "some 'quoted string'"
       ["some" "" "string"] "some\t\"\"\nstring"
       ["some 'embedded' quotes"] "\"some 'embedded' quotes\"")

  (try
    (split-quoted "unmatch'd quotes")
    (catch IllegalArgumentException e
      (is (= "Unmatched quotes" (.getMessage e))))))



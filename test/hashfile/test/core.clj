(ns hashfile.test.core
	(:use [hashfile.core] :reload)
	(:use [clojure.test])
	(:import [java.io File RandomAccessFile])
)

(deftest simple-test
	(is (= TABLE-SIZE 2000))
	(is (= HASH-FNAME "hash.bin"))
	(set-table-size 20)
	(set-hash-fname "newfile.bin")
	(is (= TABLE-SIZE 20))
	(is (= HASH-FNAME "newfile.bin"))
)

(deftest pad-test
	(is (= (count (str-pad-right 20 "word" " ")) 20))
	(is (= (str-pad-right 20 "word" " ") "word                "))
)

(deftest hash-file-test
	(init-files)
	(let [test-record {:occupied true :word "word" :value 1.0234 :collision false} blank-record (empty-record)]
		(is (= (:occupied (get-record "word")) false))
		(is (= (count (:word (get-record "word"))) 20))
		(is (= (.compareTo (:word (get-record "word")) (:word blank-record)) 0))
		(put-record test-record)
		(is (= (:word (get-record (:word test-record))) (str-pad-right 20 "word" " ")))
		(is (> 0.001 (Math/abs (- (:value (get-record (:word test-record))) (:value test-record)))))
		(is (= (:occupied (get-record (:word test-record))) true))
		(is (= (:occupied (get-record "hello")) false)))
)

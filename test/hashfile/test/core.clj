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

(deftest oflow-test
	(with-open [file (RandomAccessFile. OFLOW-FNAME "rw")]
	(let [
		rec1 (new-record "this" 1.0 false false)
		rec2 (new-record "is" 2.0 false false)
		rec3 (new-record "a" 3.0 false false)
		rec4 (new-record "test" 4.0 false false)]
		(write-a-record file rec1)
		(write-a-record file rec2)
		(write-a-record file rec3)
		(write-a-record file rec4)
		(.seek file 0)
		(is (= (:word rec3) (:word (linear-file-search file (:word rec3)))))))
)

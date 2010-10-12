(ns hashfile.core
	(:import [java.io File RandomAccessFile])
	(:gen-class)
)

(declare put-record hash-loc at-loc ord-char-at write-a-record read-a-record str-pad-right)

;parameters
(def HASH-FNAME "hash.bin")
(def OFLOW-FNAME "oflow.bin")
(def TABLE-SIZE 2000)
(def MAX-WORD-LEN 20)
(def INT-SIZE 4)
(def BOOL-SIZE 1)
(def FLOAT-SIZE 4)
(def RECORD-SIZE (+ 1 (* 2 MAX-WORD-LEN) FLOAT-SIZE 1))

;parameter setting
(defn set-table-size [size] (def TABLE-SIZE size))

(defn set-record-size [size] (def  RECORD-SIZE size))

(defn set-hash-fname [name] (def HASH-FNAME name))

(defn set-oflow-fname [name] (def OFLOW-FNAME name))

;helpers
(defn ord-char-at
	[word i]
	(int (.charAt word i))
)

(defn str-pad-right
	[num word pad] 
	(if (> num (.length word)) 
		(recur num (str word pad) pad)
		(str word))
)

(defn empty-record
	[]
	{:occupied false :collision false :word (str-pad-right MAX-WORD-LEN "-" "-") :value 0.0}
)

;hashing
(defn hash-loc
	[word i total]
	(if (< i (count word))
		(recur word (inc i) (rem (* 33 (+ 1 total (ord-char-at word i))) TABLE-SIZE))
		(rem total TABLE-SIZE))
)

(defn at-loc
	[word]
	(* RECORD-SIZE (hash-loc word 0 0))
)

;file handling
(defn read-n-chars
	[file s n ctr]
	(if (< ctr (- n 1)) 
		(recur file (str s (.readChar file)) n (inc ctr))
		(str s (.readChar file)))
)

(defn write-n-chars
	[file s n ctr]
	(.writeChar file (int (.charAt s ctr)))
	(if (< ctr (- n 1))
		(recur file s n (inc ctr)))
)

(defn write-a-record
	[file data]
	(.writeBoolean file (:occupied data))
	(.writeBoolean file (:collision data))
	(write-n-chars file (:word data) MAX-WORD-LEN 0)
	(.writeFloat file (:value data))
)

(defn read-a-record
	[file]
	{:occupied (.readBoolean file) :collision (.readBoolean file) :word (read-n-chars file "" MAX-WORD-LEN 0) :value (.readFloat file)}
)

(defn put-record
	[rec-in]
	(let [new-word (str-pad-right MAX-WORD-LEN (:word rec-in) " ")]
	(with-open [file (RandomAccessFile. HASH-FNAME "rw")]
		(.seek  file (at-loc new-word))
		(let [rec-read (read-a-record file)]
		(if (or (not (:occupied rec-read)) (= (:word rec-read) new-word))
			(do (.seek file (at-loc new-word)) (write-a-record file (assoc rec-in :word new-word)))
			(do (with-open [overflow (RandomAccessFile. OFLOW-FNAME "rw")]	
				(.seek overflow (.length overflow))
				(write-a-record overflow (assoc rec-in :word new-word))))))))
)

(defn get-record
	[word]
	(with-open [file (RandomAccessFile. HASH-FNAME "r")]
		(.seek file (at-loc (str-pad-right MAX-WORD-LEN word " ")))
		(read-a-record file))
)

(defn write-n-records
	[file record n ctr]
	(write-a-record file record)
	(if (< ctr (- n 1))
		(recur file record n (inc ctr)))
)

(defn zero-hash-file 
	[]
	(let [record (empty-record)]
	(with-open [file (RandomAccessFile. HASH-FNAME "rw")]
		(.seek file 0)
		(write-n-records file record TABLE-SIZE 0)))
)

(defn init-files 
	[]
	(.delete (File. HASH-FNAME))
	(.delete (File. OFLOW-FNAME))
	(zero-hash-file)
)






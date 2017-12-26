;; Copyright 2010-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


(in-package :language-model)

(defparameter +darpa-header+ "Darpa Trigram LM"
  "The header string at the beginning of each binary DMP LM.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Representation of the DMP ngram model ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* ngram-lm-dmp (ngram-lm)
  ((version nil ia)
   (filename nil ia)
   (1gram-count nil ia)
   (2gram-count nil ia)
   (3gram-count nil ia)
   ;; 1-gram probs and back-off weights
   (1gram-probs nil ia)
   (1gram-bo nil ia)   
   (2gram-bounds nil ia)
   ;; These are in the 2-gram section of the file
   (2gram-index nil ia)
   (2gram-prob-index nil ia)
   (2gram-bo-index nil ia)
   (3gram-bounds nil ia)
   ;; These are in the 3-gram section of the file
   (3gram-index nil ia)
   (3gram-prob-index nil ia)
   ;; These have the actual floating point numbers...
   (2gram-probs nil ia)
   (2gram-bo nil ia)
   (3gram-probs nil ia)
   ;; I'm not sure what this one is for...
   (3gram-seg nil ia))
  (:name-prefix "ngram-lm-dmp" "-")
  (:documentation "A conditional LM where the prob of a word depends only on the previous n words."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementing the standard n-gram interface ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod log-1gram-prob ((lm ngram-lm-dmp) 1gram)
  (let ((word (first 1gram)))
    (aref (ngram-lm-dmp-1gram-probs lm) (gethash word (vocab-table lm)))))

(defmethod log-2gram-prob-internal ((lm ngram-lm-dmp) 2gram)
  (let* ((word1 (first 2gram))
	 (word2 (second 2gram))
	 (index (get-2gram-index-internal lm word1 word2))
	 (prob-index (if index
			 (aref (ngram-lm-dmp-2gram-prob-index lm) index)
			 nil)))
    (if prob-index
	(aref (ngram-lm-dmp-2gram-probs lm) prob-index)
	nil)))

(defmethod log-3gram-prob-internal ((lm ngram-lm-dmp) 3gram)
  (let* ((word1 (first 3gram))
	 (word2 (second 3gram))
	 (word3 (third 3gram))
	 (index (get-3gram-index-internal lm word1 word2 word3)))
    (if index
	(aref (ngram-lm-dmp-3gram-probs lm) index)
	nil)))

(defmethod 1gram-bow-internal ((lm ngram-lm-dmp) 1gram)
  (let ((word (first 1gram)))
    (aref (ngram-lm-dmp-1gram-bo lm) (gethash word (vocab-table lm)))))

(defmethod 2gram-bow-internal ((lm ngram-lm-dmp) 2gram)
  (let* ((word1 (first 2gram))
	 (word2 (second 2gram))
	 (index (get-2gram-index-internal lm word1 word2))
	 (bow-index (when index (aref (ngram-lm-dmp-2gram-bo-index lm) index))))
    (if bow-index
	(aref (ngram-lm-dmp-2gram-bo lm) bow-index)
	nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helper functions -- to help find all the probs and bows ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2 GRAMS
(defmethod get-2gram-search-bounds-internal ((lm ngram-lm-dmp) word)
  (let* ((id (gethash word (vocab-table lm)))
	 (start (when id (aref (ngram-lm-dmp-2gram-bounds lm) id)))
	 (end (when id (aref (ngram-lm-dmp-2gram-bounds lm) (1+ id)))))
    (values start end)))

(defmethod get-2gram-index-internal ((lm ngram-lm-dmp) word1 word2)
  (multiple-value-bind (low high)
      (get-2gram-search-bounds-internal lm word1)
    (if (and low high)
	(binary-search-array (ngram-lm-dmp-2gram-index lm) (gethash word2 (vocab-table lm)) :low low :high high)
	nil)))

;;; 3 GRAMS
(defmethod get-3gram-search-bounds-internal ((lm ngram-lm-dmp) word1 word2)
  (let* ((index (get-2gram-index-internal lm word1 word2))
	 (low (when index (aref (ngram-lm-dmp-3gram-bounds lm) index)))
	 (high (when index (aref (ngram-lm-dmp-3gram-bounds lm) (1+ index)))))
    (values low high)))

(defmethod get-3gram-index-internal ((lm ngram-lm-dmp) word1 word2 word3)
  (multiple-value-bind (low high)
      (get-3gram-search-bounds-internal lm word1 word2)
    (if (and low high)
	(let ((word3-id (gethash word3 (vocab-table lm))))
	  ;;(format t "L: ~:D   H:  ~D.  TARGET: ~:D~%" low high word3-id)
	  ;;(format t "~{~:D ~}~%" (coerce (subseq (ngram-lm-dmp-3gram-index lm) low high) 'list))
	  (binary-search-array (ngram-lm-dmp-3gram-index lm) word3-id :low low :high high))
	nil)))
  

;; HELPER FUNCTIONS

(defun binary-search-array (array value &key (low 0) high)
  (declare (optimize (speed 3))
	   ;;((simple-array fixnum) array)
	   (simple-array array)
	   (fixnum low value)
	   ((or fixnum null) high))	   
  (unless high
    (setf high (length array)))
  (if (< high low)
      nil
      (let* ((middle (floor (/ (+ low high) 2)))
	     (middle-value (aref array middle)))
	(cond ((= middle-value value)
	       middle)
	      ((> middle-value value)
	       (binary-search-array array value :low low :high (1- middle)))
	      ((< middle-value value)
	       (binary-search-array array value :low (1+ middle) :high high))
	      (t (error "Should never reach this"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Converting among byte, integer, etc. representations ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given size.  Size is the number of BYTES."
  (ldb (byte size 0) value))

(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given size.  Size is the number of BYTES."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defun bytes-to-integer (byte-array &optional swap-endianness)
  "Converts an array of bytes to an integer.  MUST BE LITTLE-ENDIAN!!"
  (when swap-endianness
    (setf byte-array (reverse byte-array)))
  (let ((int 0))
    (loop for byte across byte-array
       for i from 0 to (length byte-array)
       for offset = (* i 8) do
	 (setf (ldb (byte 8 offset) int) byte))
    int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Reading/skipping headers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-header-entry (bytes offset &key swap-endianness debug)
  "Read a single header entry from a binary DMP file.  Returns the length in bytes of the header
   that we just read, but not the header itself.  The header itself is discarded."
  (let* ((entry-length (bytes-to-integer (subseq bytes offset  (+ offset 4)) swap-endianness))
	 (header-string (map 'string 'code-char (subseq bytes (+ offset 4) (+ offset 4 entry-length)))))
    (when (> entry-length 0)
      (setf header-string (subseq header-string 0 (1- entry-length))))
    (when debug (format t "HEADER: ~A~%" header-string))
    entry-length))

(defun read-all-headers (bytes offset &key swap-endianness)
  "Read all the header entries from a binary DMP file.  Returns the length in bytes of the headers
   that we just read, but not the headers themselves.  The headers themselves are discarded."
  (loop until nil
     for header-length = (read-header-entry bytes offset :swap-endianness swap-endianness) do
       (if (= header-length 0)
	   (return-from read-all-headers (+ offset 4))
	   (setf offset (+ offset header-length 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Helper functions to read sequences of ints and floats from byte arrays. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subseq2 (seq begin &optional length)
  "Convenience function that lets us call SUBSEQ with an offset and length, rather than a begin and end index.
   This makes the code in this file much cleaner."
  (subseq seq begin (+ begin length)))

(defun read-bytes-to-integer (byte-array offset byte-count &optional swap-endianness)
  "Read 'byte-count' bytes from the byte array and convert those bytes into an integer.
   Return the integer we read."
  (bytes-to-integer (subseq2 byte-array offset byte-count) swap-endianness))

(defmacro read-bytes-to-integer-incrementing (byte-array offset byte-count &optional swap-endianness)
  "Read 'byte-count' bytes from the byte array and convert those bytes into an integer.
   After doing this, increment the offset variable.  Return the integer we read."
  `(let ((int (bytes-to-integer (subseq2 ,byte-array ,offset ,byte-count) ,swap-endianness)))
     (incf ,offset ,byte-count)
     int))

(defmacro read-n-floats (bytes n offset swap-endianness)
  "A macro to read n floating point numbers from an array of bytes ('bytes'), beginning from offset.
   The functions this calls convert the bytes to floats."
  `(let ((array (make-array ,n)))
     (loop for i from 0 below ,n
      for int = (read-bytes-to-integer-incrementing ,bytes ,offset 4 ,swap-endianness)
      for prob = (ieee-floats:decode-float32 int) do
	  (setf (aref array i) prob)
	  (when (and debug (= (mod i 1000) 0))
	    (format t "~3:D. float: ~F~%" i prob)))
     array))

(defmacro read-n-ints (bytes n offset swap-endianness)
  "A macro to read n integers from an array of bytes ('bytes'), beginning from offset.
   The functions this calls convert the bytes to integers."
  `(let ((array (make-array ,n)))
     (loop for i from 0 below ,n
      for int = (read-bytes-to-integer-incrementing ,bytes ,offset 4 ,swap-endianness) do
	  (setf (aref array i) int)
	  (when (and debug (= (mod i 1000) 0))
	    (format t "~3:D. int: ~:D~%" i int)))
     array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; The main functions for reading binary DMP language models ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; It's also possible to memory map the probabilities...
;;;; To implement that see:
;;;; http://blog.viridian-project.de/2009/07/24/sbcl-getting-started-with-mmap/
;;;; and:
;;;; (apropos "sap" :sb-sys)

(defun ngram-model-dmp-read (filename &key debug)
  "The main function for reading binary DMP language models."
  (let ((bytes nil)
	(byte-counter 0)
	(swap-endianness nil)
	(lm (make-instance 'ngram-lm-dmp)))
    (with-open-file (f filename :direction :input :element-type '(unsigned-byte 8))
      (setf bytes (make-array (file-length f)))
      (read-sequence bytes f))
    (when debug (format t "File length (bytes): ~:D~%" (length bytes)))
    (let* ((header-length (bytes-to-integer (subseq bytes 0 4))))
      ;; If it's not equal, then we either have the wrong endianness, or the file is corrupt/invalid
      (when (not (= header-length (+ (length +darpa-header+) 1)))
	(setf header-length (bytes-to-integer (subseq bytes 0 4) t))
	(setf swap-endianness t)
	;; If it's still not equal, then the file is invalid
	(when (not (= header-length (+ (length +darpa-header+) 1)))
	  (error "Wrong magic header size number ~:d: ~a is not a dump file~%" header-length filename)))
      (when debug (format t "Header length (bytes): ~:D~%" header-length))
      ;; Read the header and the filename
      (let* ((header (map 'string 'code-char (subseq bytes 4 (+ 4 header-length))))
	     (filename-begin (+ header-length 4))
	     (filename-length (bytes-to-integer (subseq bytes filename-begin (+ filename-begin 4)) swap-endianness))
	     (filename (map 'string 'code-char (subseq bytes (+ filename-begin 4) (+ filename-begin 4 filename-length)))))
	(assert (= header-length (+ (length +darpa-header+) 1)))
	(assert (string= +darpa-header+ header :end1 (1- header-length) :end2 (1- header-length)))
	(setf (ngram-lm-dmp-filename lm) filename)
	(when debug (format t "Header: ~A, header length: ~A~%" header header-length))
	(when debug (format t "Filename: ~A, filename length: ~A~%" filename filename-length))
	(setf byte-counter (+ filename-begin 4 filename-length))
	(setf byte-counter (read-dmp-version-and-counts :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	(when debug (format t "Byte counter: ~:D~%" byte-counter))
	;; First read the 1gram probs, etc.
	(setf byte-counter (read-dmp-1grams :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	;; And the 2- and 3- gram prob indicies.
	(setf byte-counter (read-dmp-2gram-indicies :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	(setf byte-counter (read-dmp-3gram-indicies :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	;; Lastly, we read the probabilties themselves...
	(setf byte-counter (read-dmp-2gram-probs-and-bo :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	(setf byte-counter (read-dmp-3gram-probs :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	(setf byte-counter (read-dmp-tseg :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	;; Read the vocab
	(setf byte-counter (read-dmp-vocab :bytes bytes :byte-counter byte-counter :swap-endianness swap-endianness :debug debug :lm lm))
	lm))))

(defun read-dmp-version-and-counts (&key bytes byte-counter swap-endianness debug lm)
  "Read the version number, and the n-gram counts."
  (let ((version-number (unsigned-to-signed (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness) 4))
	(time-stamp nil)
	(1gram-count 0)
	(2gram-count 0)
	(3gram-count 0)
	(n nil))
    (if (<= version-number 0)
	(progn
	  (setf time-stamp (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	  (setf byte-counter (read-all-headers bytes byte-counter :swap-endianness swap-endianness))
	  (setf 1gram-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness)))
	(setf 1gram-count version-number))
    (setf 2gram-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
    (setf 3gram-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
    (when debug (format t "Version: ~A.  Time stamp: ~D~%" version-number time-stamp))
    (format t "ngrams 1=~:d, 2=~:d, 3=~:d~%" 1gram-count 2gram-count 3gram-count)
    (cond ((> 3gram-count 0)
	   (setf n 3))
	  ((> 2gram-count 0)
	   (setf n 2))
	  (t (setf n 1)))
    (setf (ngram-lm-dmp-1gram-count lm) 1gram-count)
    (setf (ngram-lm-dmp-2gram-count lm) 2gram-count)
    (setf (ngram-lm-dmp-3gram-count lm) 3gram-count)
    (setf (order lm) n))
  byte-counter)

(defun read-dmp-1grams (&key bytes byte-counter swap-endianness debug lm)
  "Read the Unigrams -- IDs, probabilities, backoff weights, and indices into the bigram probs"
  (let ((1gram-count (ngram-lm-dmp-1gram-count lm)))
    (setf (ngram-lm-dmp-1gram-probs lm) (make-array 1gram-count :element-type 'single-float))
    (setf (ngram-lm-dmp-1gram-bo lm) (make-array 1gram-count :element-type 'single-float))
    (setf (ngram-lm-dmp-2gram-bounds lm) (make-array 1gram-count :element-type '(unsigned-byte 32)))
    (loop for i from 0 to 1gram-count
       for mapping-id = (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness)
       for prob = (ieee-floats:decode-float32 (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
       for bo-weight = (ieee-floats:decode-float32 (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
       for bigram-index = (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness) do
	 ;; This assertion seems to fail at the end, the last value is (-1 as unsigned int?) the sentinal?
	 (unless (= mapping-id 4294967295)
	   (assert (= i mapping-id))
	   (setf (aref (ngram-lm-dmp-1gram-probs lm) mapping-id) prob)
	   (setf (aref (ngram-lm-dmp-1gram-bo lm) mapping-id) bo-weight)
	   (setf (aref (ngram-lm-dmp-2gram-bounds lm) mapping-id) bigram-index))
	 (when (and debug (= (mod i 1000) 0))
	   (format t "1-gram:  ID: ~:D, Prob: ~F, BO weight: ~F, BG-index: ~:D~%" mapping-id prob bo-weight bigram-index))))
  byte-counter)
  
(defun read-dmp-2gram-indicies (&key bytes byte-counter swap-endianness debug lm)
  "Read the bigrams -- Read 16-bit IDs/pointers to: the words, the index of the probability, the index of the back-off weight, and an index into the trigram table"
  (let ((2gram-count (ngram-lm-dmp-2gram-count lm)))
    (setf (ngram-lm-dmp-2gram-index lm) (make-array (1+ 2gram-count) :element-type '(unsigned-byte 32)))
    (setf (ngram-lm-dmp-2gram-prob-index lm) (make-array (1+ 2gram-count) :element-type '(unsigned-byte 32)))
    (setf (ngram-lm-dmp-2gram-bo-index lm) (make-array (1+ 2gram-count) :element-type '(unsigned-byte 32)))
    (setf (ngram-lm-dmp-3gram-bounds lm) (make-array (1+ 2gram-count) :element-type '(unsigned-byte 32)))
    (loop for i from 0 to 2gram-count
       for word-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness)
       for prob-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness)
       for bo-weight-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness)
       for trigram-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness) do
	   (setf (aref (ngram-lm-dmp-2gram-index lm) i) word-index)
	   (setf (aref (ngram-lm-dmp-2gram-prob-index lm) i) prob-index)
	   (setf (aref (ngram-lm-dmp-2gram-bo-index lm) i) bo-weight-index)
	   (setf (aref (ngram-lm-dmp-3gram-bounds lm) i) trigram-index)
	 (when (and debug (= (mod i 100000) 0))
	   (format t "2-gram:  index: ~8:D word-index: ~8:D, prob-index: ~8:D, bow-index: ~8:D, 3-gram-index: ~8:D~%" i word-index prob-index bo-weight-index trigram-index))))
  byte-counter)
  
(defun read-dmp-3gram-indicies (&key bytes byte-counter swap-endianness debug lm)
  "Read the tri-grams -- Read 16-bit IDs for the words, and 16-bit indicies into the actual probabilities"
  (let ((3gram-count (ngram-lm-dmp-3gram-count lm)))
    (setf (ngram-lm-dmp-3gram-index lm) (make-array 3gram-count :element-type '(unsigned-byte 32)))
    (setf (ngram-lm-dmp-3gram-prob-index lm) (make-array 3gram-count :element-type '(unsigned-byte 32)))
    ;; This only goes up to 3gram-count - 1
    (loop for i from 0 below 3gram-count
       for word-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness)
       for prob-index = (read-bytes-to-integer-incrementing bytes byte-counter 2 swap-endianness) do
	 (setf (aref (ngram-lm-dmp-3gram-index lm) i) word-index)
	 (setf (aref (ngram-lm-dmp-3gram-prob-index lm) i) prob-index)
	 (when (and debug (= (mod i 100000) 0))
	   (format t "3-gram:  index: ~8:D w-index: ~8:D, Prob-index: ~8:D~%" i word-index prob-index))))
  byte-counter)

(defun read-dmp-2gram-probs-and-bo (&key bytes byte-counter swap-endianness debug lm)
  "Read the BIGRAM probabilities (32 bit floats) and backoff weights."
  (let ((2gram-count (ngram-lm-dmp-2gram-count lm)))
    (when (> 2gram-count 0)
      (let* ((bigram-prob-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	     (bigram-probs (read-n-floats bytes bigram-prob-count byte-counter swap-endianness)))
	(when debug (format t "2gram prob count: ~:D~%" bigram-prob-count))
	(setf (ngram-lm-dmp-2gram-probs lm) bigram-probs)
	(assert (= (length bigram-probs) bigram-prob-count)))
      (let* ((bow-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	     (bo-weights (read-n-floats bytes bow-count byte-counter swap-endianness)))
	(setf (ngram-lm-dmp-2gram-bo lm) bo-weights)
	(when debug (format t "BOW count: ~:D~%" bow-count))
	(assert (= (length bo-weights) bow-count)))))
  byte-counter)

(defun read-dmp-3gram-probs (&key bytes byte-counter swap-endianness debug lm)
  "Read the TRIGRAM *probabilities only* (32 bit floats)"
  (let ((3gram-count (ngram-lm-dmp-3gram-count lm)))
    (when (> 3gram-count 0)
      (let* ((trigram-prob-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	     (trigram-probs (read-n-floats bytes trigram-prob-count byte-counter swap-endianness)))
	(setf (ngram-lm-dmp-3gram-probs lm) trigram-probs)
	(when debug (format t "3-gram prob count: ~:D~%" trigram-prob-count))
	(assert (= (length trigram-probs) trigram-prob-count)))))
  byte-counter)
  
(defun read-dmp-tseg (&key bytes byte-counter swap-endianness debug lm)
  "What is this?  The segmentation of the trigram probabilities?"
  (let* ((tseg-base-size (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	 (trigram-index (read-n-ints bytes tseg-base-size byte-counter swap-endianness)))
    (setf (ngram-lm-dmp-3gram-seg lm) trigram-index)
    (assert (= (length trigram-index) tseg-base-size))
    (when debug (format t "~:D~%" tseg-base-size))
    (format t "~:D~%" tseg-base-size))
  byte-counter)

(defun read-dmp-vocab (&key bytes byte-counter swap-endianness debug lm)
  "Down at the bottom is the vocab itself"
  (let* ((1gram-count (ngram-lm-dmp-1gram-count lm))
	 (vocab-byte-count (read-bytes-to-integer-incrementing bytes byte-counter 4 swap-endianness))
	 (vocab-bytes (map 'list 'code-char (subseq2 bytes byte-counter vocab-byte-count)))
	 (word-count (count #\Nul vocab-bytes))
	 (words (map 'vector (lambda (x) (coerce x 'string)) (split-sequence:split-sequence #\Nul vocab-bytes :remove-empty-subseqs t))))
    (when debug (format t "WORD BYTES: ~D~%" vocab-byte-count))
    (assert (= (length vocab-bytes) vocab-byte-count))
    (assert (= word-count 1gram-count (length words)))
    (set-vocab lm words))
  byte-counter)




  

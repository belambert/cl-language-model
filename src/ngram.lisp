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

;;;; NOTE:
;;;; It seems that saving the word sequences as vectors doesn't really save us much RAM space
;;;; over lists (I guess because they are so short), so the overhead of keeping a pointer to the
;;;; end of the array destroys any gain we get from using arrays rather then lists
;;;; So, it seems there aren't any quick and easy ways to reduce the memory footprint, 
;;;; without major changes.  We could perhaps make some drastic changes to how everything is stored
;;;; in memory to get some gains... e.g. totally changing the hash table keys strategy, or
;;;; getting rid of hash tables entirely, or memory mapping the LM as a DMP file
;;;; For now, it's probably easier to just use smaller LMs or bigger machines.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Class definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *default-ngram-table-size*
  16
  "The initial size of the ngram LM hash tables.  This is used for each of the 
   prob table, backoff table, and sampling table.  So, the initial value really shouldn't
   be bigger than any one of those.  Thus, if we're not doing sampling...")

;; These are easily going to take up a huge amount of memory, e.g. 1GB,
;; since the probability and backoff tables will be very large (e.g. 3 million entries)
(defclass* ngram-lm-arpa (ngram-lm)
  ((probability-table (make-hash-table :test 'equalp :size *default-ngram-table-size*) i :reader ngram-lm-arpa-probability-table)
   (backoff-table     (make-hash-table :test 'equalp :size *default-ngram-table-size*) i :reader ngram-lm-arpa-backoff-table)
   (sampling-table    (make-hash-table :test 'equalp) i :reader ngram-lm-arpa-sampling-table)
   (filename nil i :reader ngram-lm-arpa-filename))
  (:documentation "A conditional LM where the prob of a word depends only on the previous n words."))

(defmethod initialize-instance :after ((lm ngram-lm-arpa) &key sampling)
  "Constructor for the n-gram model.  This reads the file, etc."
  (read-arpa-model (ngram-lm-arpa-filename lm) lm)
  ;; Put it into the vocab?
  (setup-ngram-vocab lm)
  ;; Sort the probs for more efficient sampling
  (when sampling
    (create-ngram-sampling-table lm)
    (sort-probability-table (ngram-lm-arpa-sampling-table lm)))
  (print-lm-stats lm)
  (setf (vocab lm) (coerce (sort (hash-table-keys (vocab-table lm)) 'string-lessp) 'vector))
  ;; Set the history length from the model's order
  (if (order lm)
      (setf (history-length lm) (1- (order lm)))
      (format t "WARNING: Unbounded history length for conditional model.~%"))
  ;; After we're done, do a full GC since we may have created a lot of garbage in the process of reading the  model
  (sb-ext:gc :full t)
  lm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Loading and initializing an n-gram model from an ARPA file ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-last-list-elt (list)
  "Remove the last elt of a list without consing (i.e. without calling SUBSEQ)."
  (declare (optimize (speed 3)))
  (let ((prev-elt nil))
    (loop for x on list do
	 (unless (cdr x)
	   (setf (cdr prev-elt) nil))
	 (setf prev-elt x))
    list))

(defun check-token-count (tokens order-of-section)
  "Make sure the number of tokens is equal to one or two more than the order of the section."
  (when (and (> (length tokens) 1) order-of-section)
    (assert (or (= (length tokens) (+ order-of-section 2))
		(= (length tokens) (+ order-of-section 1)))))) ;; no backoff for last order
    
(defun create-ngram-sampling-table (lm)
  "Create and populate a sampling table for the language model.  (This takes up a lot of memory?)"
  (loop for word-seq being the hash-keys in (ngram-lm-arpa-probability-table lm)
     for probability = (gethash word-seq (ngram-lm-arpa-probability-table lm)) do
       (push (list word-seq (expt 10 probability))
	     (gethash (subseq word-seq 0 (1- (length word-seq)))
		      (ngram-lm-arpa-sampling-table lm)))))

(defun setup-ngram-vocab (lm)
  (loop for word-seq being the hash-keys in (ngram-lm-arpa-probability-table lm) do
       (dolist (word word-seq)
	 (setf (gethash word (vocab-table lm)) t))))

(defun extract-word-sequence (tokens bow-p)
  "Get rid of probability and bow, so we just have the tokens left, and
   get shared versions of each string to save memory"
  (pop tokens)
  (when bow-p (setf tokens (remove-last-list-elt tokens)))
  (setf tokens (mapcar 'get-cached-string tokens))
  tokens)

(defun ngram-section-start-p (line)
  (and (> (length line) 0) (char= (aref line 0) #\\) (string-not-equal line "\\end\\") (string-not-equal line "\\data\\")))

(defun extract-order-from-line (line)
  (when (ngram-section-start-p line)
    (let* ((order-char (subseq line 1 2)) ;; just take the 2nd character
	   (order (parse-integer order-char)))
      order)))
  
(defun read-arpa-model (filename lm)
  "Read an ARPA LM file into a list of structures, one for each n-gram in the file.  This 'only' goes up to 9-grams."
  (let ((order-of-section nil))
    (declare ((or null fixnum) order-of-section))
    (format t "Reading ARPA LM file: ~A.~%" filename)
    (do-lines-fast (line filename)
      ;; This reads the beginning of a section...that is e.g.: \2=gram
      (if (ngram-section-start-p line)
	  (setf order-of-section (extract-order-from-line line))	  
	  ;; Now read a regular line...
	  (let ((tokens (split-sequence-if (lambda (x) (or (char= x #\Tab) (char= x #\Space))) line)))
	    (declare (list tokens))
	    (when (and (> (length tokens) 1) order-of-section)
	      ;; Do a sanity check on the number of tokens
	      (check-token-count tokens order-of-section)	      
	      (let* ((bow-p (= (length tokens) (+ order-of-section 2))) ;; is there a bow?		      
		     (probability-token (elt tokens 0))
		     (probability (if (equal probability-token "-99") -99 (parse-number:parse-real-number probability-token)))
		     (last-token (when bow-p (alexandria:last-elt tokens)))
		     (backoff-weight (when bow-p (parse-number:parse-number last-token)))
		     (word-seq (extract-word-sequence tokens bow-p)))
		(declare (number probability) ((or number null) backoff-weight))
		;; Save the probability and back-off weight
		(setf (gethash word-seq (ngram-lm-arpa-probability-table lm)) probability)
		(when backoff-weight
		  (setf (gethash word-seq (ngram-lm-arpa-backoff-table lm)) backoff-weight)))))))
      ;; Just take the last section order as the overall order (which may not be correct)
      (setf (order lm) order-of-section)))

(defun print-lm-stats (lm)
  "Print the order, vocab size, and number of 1-,2-, and 3-grams of the given model."
  (format t "N-gram counts for ~a-gram LM: vocab size=~:d, 1grams=~:d, 2grams=~:d, 3grams=~:d~%  ~%"
	  (order lm)
	  (hash-table-count (vocab-table lm))
	  (count-if (lambda (x) (= (length x) 1)) (hash-table-keys (ngram-lm-arpa-probability-table lm)))
	  (count-if (lambda (x) (= (length x) 2)) (hash-table-keys (ngram-lm-arpa-probability-table lm)))
	  (count-if (lambda (x) (= (length x) 3)) (hash-table-keys (ngram-lm-arpa-probability-table lm)))))

(defun sort-probability-table (table)
  "Sort the probabilities for a particular history, for more efficient sampling."
  (loop for key being the hash-keys of table
       for prob-list = (gethash key table) do
       (setf (gethash key table)
	     (make-array (length prob-list) :initial-contents (sort prob-list '> :key 'second)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Sampling an n-gram model (i.e. random generation)  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sample-ngram (n given lm &key (verbose nil))
  "Sample an n-gram LM given some history.  This is the main function to use for sampling from an n-gram model."
  (setf n (min n (length given)))
  (when (> (length given) n)
    (setf given (subseq given (max 0 (- (length given) n)) (length given)))) ;; removed a -1 from this subtraction... 
  (when verbose (format t "Using history: ~{~A ~}~%" given))
  (let* ((prob-array (gethash given (ngram-lm-arpa-sampling-table lm)))
	 (random-number (random 1.0))
	 (accumulated-probability-mass 0.0)
	 (total-mass-available (when verbose (get-total-probability-mass prob-array))))
    (when verbose (format t "RAND: ~F, TOTAL: ~F~%" random-number total-mass-available))
    (when prob-array
      (loop for pair across prob-array do
	   (incf accumulated-probability-mass (second pair))
	   (when (> accumulated-probability-mass random-number)
	     (setf pair (first (last (first pair))));; confusing... get's the last element of the first element
	     (when verbose (format t "GENERATED: ~A~%" pair))
	     (return-from sample-ngram pair))))
    ;; if we got this far, it means we need to back-off?
    ;; we get here if we don't have a n-gram array in the h.t., or we ran out of probability...
    (when verbose (format t "Not enough prob mass.  Need to back-off.  [RAND: ~F; MASS: ~F]~%" random-number accumulated-probability-mass))
    (if (= n 0)
	(sample-ngram n given lm)
	(sample-ngram (1- n) (subseq given 1 (length given)) lm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implementing the standard n-gram interface ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod log-1gram-prob ((lm ngram-lm-arpa) 1gram)
  (gethash 1gram (ngram-lm-arpa-probability-table lm)))

(defmethod log-2gram-prob-internal ((lm ngram-lm-arpa) 2gram)
  (gethash 2gram (ngram-lm-arpa-probability-table lm)))

(defmethod log-3gram-prob-internal ((lm ngram-lm-arpa) 3gram)
  (gethash 3gram (ngram-lm-arpa-probability-table lm)))

(defmethod 2gram-bow-internal ((lm ngram-lm-arpa) 2gram)
  (gethash 2gram (ngram-lm-arpa-backoff-table lm)))

(defmethod 1gram-bow-internal ((lm ngram-lm-arpa) 1gram)
  (gethash 1gram (ngram-lm-arpa-backoff-table lm)))

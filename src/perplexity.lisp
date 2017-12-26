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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Probability streams   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prob-stream-ppl (s &key (base 2) (already-log-probs nil))
  "Compute and print the PPL,etc. of a probability stream."
  (let ((stats (prob-stream-ppl-stats s :base base :already-log-probs already-log-probs)))
    (prob-stream-stats-ppl stats)))

(defun prob-stream (data lm &key (probability-function 'prob-of-lm-example))
  "Get the n-gram model's prob stream for the loaded data."
  (let ((prob-list '()))
    (loop for example in data
       for prob = (coerce (funcall probability-function lm example) 'single-float) do
	 (assert (> prob 0.0))
	 (push prob prob-list))
    (make-array (length prob-list) :element-type 'single-float :initial-contents (nreverse prob-list))))

(defun history-prob-stream (data lm)
  "Get the n-gram model's prob stream for the loaded data."
  (prob-stream data lm :probability-function 'prob-of-history))

(defun lm-example-prob-stream (data lm)
  "Get the n-gram model's prob stream for the loaded data."
  (prob-stream data lm :probability-function 'prob-of-lm-example))

(defun whole-sentence-log-prob-stream (sentences sentence-model)
  "Get a stream of log probabilities for a sequence of sentences."
  (let* ((log-prob-stream '())
	 (total-word-count 0))
    (loop for sentence in sentences
       for word-count = (1- (length (sentence-segmented-words sentence))) do  ;; subtract one, so we don't count the begin sentence token
	 (let* ((log-prob (log-prob-of-sentence sentence-model sentence)))
	   (push log-prob log-prob-stream)
	   (incf total-word-count word-count)))
    (values (nreverse log-prob-stream) total-word-count)))

(defun read-probability-stream (filename)
  "Read a probability stream of the format given by Roni."
  (let ((prob-stream (make-array 10 :fill-pointer 0 :adjustable t)))
    (with-open-file (file filename)
      (loop for line = (read-line file nil nil)
	 until (not line)
	 do
	   (let ((prob (read-from-string line)))
	     (vector-push-extend prob prob-stream)
	     )))
    prob-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Perplexity computation   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct prob-stream-stats
  "Represents PPL, LL, etc."
  ll    ;; log likelihood
  l     ;; likelihood
  all   ;; average log likelihood
  al    ;; average likelihood
  ppl)  ;;perplexity

(defun prob-stream-ppl-stats (s &key (base 2) (already-log-probs nil))
  "Takes a list of probabilities and computes various info theory stats on it."
  (let ((ll 0.0d0))
    ;; add up the log probs to get the total log-likelihood
    (loop for p across s do
      (if already-log-probs
	  (incf ll p)
	  (incf ll (log p base))))
    ;; Compute the stats we're interested in
    (let* ((all (if (/= (length s) 0)
		    (/ ll (length s))
		    0.0))
	   (likelihood (expt base ll))
	   (ppl (expt base (- all)))
	   (al (expt base all)))
      (make-prob-stream-stats :ll ll
			      :l likelihood
			      :all all
			      :al al
			      :ppl ppl))))

(defun print-prob-stream-ppl (s &key (base 2) (already-log-probs nil) (verbose nil))
  "Compute and print the PPL,etc. of a probability stream."
  (let ((stats (prob-stream-ppl-stats s :base base :already-log-probs already-log-probs)))
    (when verbose
      (format t "LL:  ~F~%" (prob-stream-stats-ll stats))
      (format t "L:   ~F~%" (prob-stream-stats-l stats))
      (format t "ALL: ~F~%" (prob-stream-stats-all stats))
      (format t "A-L: ~F~%" (prob-stream-stats-al stats)))
    (format t "PPL: ~F~%" (prob-stream-stats-ppl stats))))

(defun print-model-ppl (lm data)
  "Given a model and some data, compute the PPL and print it."
  (let* ((ps (prob-stream data lm)))
    (format t "PPL:~%")
    (print-prob-stream-ppl ps)))

(defun model-ppl (lm data)
  "Given a model and some data, print the"
  (let* ((ps (prob-stream data lm)))
    (prob-stream-ppl ps)))

(defun print-whole-sentence-prob-stream-ppl (prob-stream word-count)
  "Print the PPL of a whole-sentence probability stream, given the stream and the total word count."
  (let* ((sll 0.0)
	 (sentence-count (length prob-stream)))
    (dolist (log-prob prob-stream)
      (incf sll log-prob))
    (let* ((awll (/ sll word-count))
	   (wppl (expt 2 (- awll))))
      (format t "Sentence count: ~:d~%" sentence-count)
      (format t "Word count:     ~:d  (includes EOS, but not BOS)~%" word-count)
      (format t "Sentence LL:    ~F~%" sll)
      (format t "Avg word LL:    ~F~%" awll)
      (format t "Word PPL:       ~F~%" wppl))))

(defun whole-sentence-prob-stream-ppl (prob-stream word-count)
  "Return the word-PPL of a sentence probability stream, given the stream and the number of words in that stream."
  (let* ((sll 0.0))
    (dolist (log-prob prob-stream)
      (incf sll log-prob))
    (let* ((awll (/ sll word-count))
	   (wppl (expt 2 (- awll))))
      wppl)))

(defun print-whole-sentence-model-ppl (sentences sentence-model)
  "Print the WSM PPL for a list of sentences."
  (multiple-value-bind (prob-stream word-count)
      (whole-sentence-log-prob-stream sentences sentence-model)
    (print-whole-sentence-prob-stream-ppl prob-stream word-count)))

(defun whole-sentence-model-ppl (sentences sentence-model)
  "Return the WSM PPL for a list of sentences."
  (multiple-value-bind (prob-stream word-count)
      (whole-sentence-log-prob-stream sentences sentence-model)
    (whole-sentence-prob-stream-ppl prob-stream word-count)))    

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


;;;; Functions for combining independent scores in a pattern LM.

(in-package :language-model)

(defun score-max (model sentence)
  "Use the maximum feature weight according to the model as the sentence's score."
  (let* ((feature-scores (map 'list (lambda (x) (aref (the (simple-array double-float) (parameters model)) (speech-parser::pattern-sparse-id x))) (speech-parser::sentence-active-sparse-patterns sentence)))
	 (max-score (if (/= (length feature-scores) 0)
			(reduce 'max feature-scores)
			0.0d0)))
    max-score))

(defun score-max-abs (model sentence)
  "Use the maximum MAGNITUDE feature weight according to the model as the sentence's score (i.e. max absolute value)."
  (let* ((feature-scores (map 'list (lambda (x) (aref (the (simple-array double-float) (parameters model)) (speech-parser::pattern-sparse-id x))) (speech-parser::sentence-active-sparse-patterns sentence)))
	 (max-score (if (/= (length feature-scores) 0)			
			(reduce 'max-abs feature-scores)
			0.0d0)))
    max-score))

;; TODO -- THIS IS VERY SLOW WHEN WE HAVE *LOTS* OF ACTIVE CONSTRUCTIONS...
(defun sum-non-overlapping (model sentence &key (max-op 'max))
  "Get the sum of non-overlapping scores for the sentence according to the model.
   Chooses matches in a greedy fashion, from largest to smallest -- map-op controls whether
   it's truly the maximum operation or the maximum magnitude with 'max or 'max-abs."
  (when (= (length (the simple-array (speech-parser::sentence-active-sparse-patterns sentence))) 0)
    (return-from sum-non-overlapping 0.0d0))
  (let* (;;(words-used (make-array (the fixnum (sentence-word-count sentence)) :initial-element nil))
	 (words-used (make-array (the fixnum (length (speech-parser::sentence-segmented-words sentence))) :initial-element nil))
	 (sparse-patterns (speech-parser::sentence-active-sparse-patterns sentence))
	 (feature-scores (make-array (length sparse-patterns) :element-type 'double-float))
	 (score-sum 0.0d0)
	 (max-score 0.0d0))
    (declare (simple-array sparse-patterns)
	     (double-float score-sum))
    (loop for sparse-pattern across (the (simple-array speech-parser::pattern-sparse) sparse-patterns)
       for id = (speech-parser::pattern-sparse-id sparse-pattern)
       for i from 0 do
	 (setf (elt feature-scores i) (aref (the (simple-array double-float) (parameters model)) id)))
    (setf max-score (reduce max-op feature-scores))
    (loop while (/= (the double-float max-score) 0.0)
       for sparse-pattern-index = (position max-score feature-scores :test '=) ;; this will arbitrarily take the first one for ties...
       for sparse-pattern = (elt sparse-patterns sparse-pattern-index)
       for pattern-begin = (speech-parser::pattern-sparse-begin sparse-pattern)
       for pattern-end = (speech-parser::pattern-sparse-end sparse-pattern) do
	 ;; if the pattern has a begin and end location
	 (if (and pattern-begin pattern-end)
	     ;; If the words aren't already used by other constructions (which we check by searching for anything that evaluates to 'true')
	     ;; then we add this pattern's score to the total score and set the matching tokens to be marked as "used".
	     (unless (find-if 'identity words-used :start pattern-begin :end pattern-end)
	       (incf score-sum max-score)
	       (fill words-used t :start pattern-begin :end pattern-end))
	     ;; Otherise, if it doesn't have a start/end, then we count it and don't mark any words as used by the pattern
	       (incf score-sum max-score))
       ;; We either just included the score in the sum, or found that the tokens were already used by another construction.
       ;; In either case, we want to set this score to zero so we don't look at it again...
	 (setf (elt feature-scores sparse-pattern-index) 0.0d0)
	 (setf max-score (reduce max-op feature-scores)))
    score-sum))

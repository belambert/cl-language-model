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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General data structures and parameters  ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-seq->word-and-history (seq)
  "Convert a word sequence into two parts: a word, and a history.  Used by the n-gram model."
  (let ((length (length seq)))
    (values (elt seq (1- length))
	    (subseq seq 0 (1- length)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; General class and methods for all LMs ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* lm ()
  ((log-base 10 ia :accessor lm-log-base	:initarg :lm-log-base "The base to use for logarithms.  We've been using 2.")
   (vocab #() ia "An array the enumerates the LM's vocab.")
   (vocab-table (make-hash-table :test 'equalp) ia "A table mapping from vocab words to their numerical IDs.")
   (vocab-size 0 ia "The size of the vocabulary.  This should be equal to the length of vocab.")
   (start-sentence-token "<s>" ir "The token that designates the start of a sentence, usually '&lts&gt'.")
   (end-sentence-token "</s>" ir "The token that designates the start of a sentence, usually '&lt/s&gt'.")
   (open-vocabulary t ir "Boolean value, whether this LM can handle OOVs.")
   (oov-token "<unk>" ir  "The desginated token that replaces OOVs, usually '&ltunk&gt'."))
  (:documentation "A language model - computes probabilities of words or sentences."))

(defmethod initialize-instance :after ((lm lm) &key)
  "The constructor for the general LM class. This ensures that the vocabulary is in a vector/array, and it loads the pre-loaded samples when specified.
   This doesn't get lispdoc'ed b/c the :after throws things off."
  (when (vocab lm) ;; Make sure the vocab is a vector
    (set-vocab lm (coerce (vocab lm) 'vector))))

(defgeneric save-model (lm filename &key) (:documentation "Save an LM to a file."))
(defgeneric substitute-oovs (lm word-list) (:documentation "Replace any OOV words with the designated OOV token."))
(defgeneric set-vocab (lm vocab) (:documentation "Set the vocab to the given array.  This also populates the vocab hash-table."))
(defgeneric in-vocab-p (lm word) (:documentation "Check if the given word is in the vocabulary."))
(defgeneric substitute-lm-example-oovs (lm lm-example) (:documentation "Substitute the OOVs in an LM example."))
(defgeneric get-word-id (lm word) (:documentation "Get the numerical ID of the given word"))
(defgeneric get-word-id-adding (lm word) (:documentation "Get the numerical ID of the given word.  And add it to the vocab if it's not already there."))
(defgeneric add-word-to-vocab (lm word) (:documentation "Add a word to the LM's vocabulary."))

(defmethod in-vocab-p ((lm lm) word)
  "Check if the given word is in the vocabulary."
  (gethash word (vocab-table lm)))

(defmethod get-word-id ((lm lm) word)
  "Check if the given word is in the vocabulary."
  (gethash word (vocab-table lm)))

(defmethod get-word-id-adding ((lm lm) word)
  "Check if the given word is in the vocabulary."
  (let ((id (get-word-id lm word)))
    (or id (add-word-to-vocab lm word))))

(defmethod add-word-to-vocab ((lm lm) word)
  "Check if the given word is in the vocabulary."
  (let ((index (vector-push-extend word (vocab lm))))
    (setf (gethash word (vocab-table lm)) index)
    index))

(defmethod substitute-oovs ((lm lm) word-list)
  "Given a word list, substitute the out-of-vocab words with the designated OOV token."
  (when (open-vocabulary lm)
    (when (find-if-not (lambda (x) (in-vocab-p lm x)) word-list)
      (setf word-list (substitute-if-not (oov-token lm) (lambda (x) (in-vocab-p lm x)) word-list))))
  word-list)

(defmethod set-vocab ((lm lm) vocab)
  "Set the vocab to the given array.  This also populates the vocab hash-table."
  (setf (vocab lm) vocab)
  (setf (vocab-size lm) (length vocab))
  (setf (vocab-table lm) (make-hash-table :test 'equalp))
  (loop for word across (vocab lm)
     for i from 0 to (length (vocab lm)) do
       (setf (gethash word (vocab-table lm)) i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Whole-sentence LMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sentence-lm (lm)
  () (:documentation "A whole-sentence LM, computes (log) probabilities of entire sentences.  
      All(?) of the other models are subclasses of this (since they can all assign a prob to a full sentence)"))

;;; We do not include a non-log prob function here, because sentence probabilities are so low.
(defgeneric log-prob-of-sentence (sentence-lm sentence)
  (:documentation "Get the log probabilities of a given sentence, according to the model."))

(defgeneric generate-sentence (sentence-lm)
  (:documentation "Randomly generate a sentence from the model."))

(defgeneric score (sentence-lm sentence &key max-feature-num)
  (:documentation "Get a 'score' for the sentence.  It need not be a log prob, but defaults to o/w.  Preferably cache any feature computation."))

(defmethod score ((lm sentence-lm) sentence &key max-feature-num)
  "If this method is not implemented, it just calls LOG-PROB-OF-SENTENCE"
  (declare (ignore max-feature-num))
  (log-prob-of-sentence lm sentence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Conditional LMs -- more specific than whole-sentence  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* conditional-lm (sentence-lm)
  ((history-length nil a)
   (order nil ia))
  (:documentation "A conditional LM model the probability of the next word, given the preceding words."))

(defgeneric prob-of-word (conditional-lm word history)
  (:documentation "Probability of next word given a list of words before it."))

(defgeneric log-prob-of-word (conditional-lm word history)
  (:documentation "Log probability of next word given a list of words before it."))

(defgeneric prob-of-history (conditional-lm history)
  (:documentation "Probability of the last word in a sequence of words, using the other words in the sequence as the history."))

(defgeneric log-prob-of-history (conditional-lm history)
  (:documentation "Log-probability of the last word in a sequence of words, using the other words in the sequence as the history."))

(defgeneric generate-word (conditional-lm history)
  (:documentation "Randomly generate the next word given the history."))

;; These two are mutually defined....  so, a model must implement one or the other, or both.
(defmethod prob-of-word ((lm conditional-lm) word history)
  "Probability of next word given a list of words before it."
  (expt (lm-log-base lm) (log-prob-of-word lm word history)))

(defmethod log-prob-of-word ((lm conditional-lm) word history)
  "Log probability of next word given a list of words before it."
  (log (prob-of-word lm word history) (lm-log-base lm)))

(defmethod prob-of-history ((lm conditional-lm) history)
  "Probability of the last word in a sequence of words, using the other words in the sequence as the history."
  (expt (lm-log-base lm) (log-prob-of-history lm history)))

(defmethod log-prob-of-history ((lm conditional-lm) history)
  "First concert the history into a word/history pair, then get the log prob."
  (multiple-value-bind (word history)
      (word-seq->word-and-history history)
    (log-prob-of-word lm word history)))

(defmethod generate-sentence ((lm conditional-lm))
  "Generate a sentence for a conditional LM.  Entails generating one word at a time."
  (declare (optimize (speed 3)))
  (let ((words (list (start-sentence-token lm)))
	(last-word (start-sentence-token lm)))
    (loop until (string-equal last-word (end-sentence-token lm)) do
	 (let ((next-word (generate-word lm words)))
	   (setf words (append words (list next-word)))
	   (setf last-word next-word)))
    words))

(defmethod log-prob-of-sentence ((lm conditional-lm) (sentence sentence))
  "Given a conditional LM and a sentence, compute the LL of the entire sentence."
  (log-prob-of-sentence lm (sentence-segmented-words sentence)))

(defmethod log-prob-of-sentence ((lm conditional-lm) (sentence vector))
  "Given a conditional LM and a sentence, compute the LL of the entire sentence."
  (setf sentence (coerce sentence 'cons))
  (let* ((log-prob-sum 0.0)
	 (words sentence))
    (unless (equal (first words) (start-sentence-token lm)) (push (start-sentence-token lm) words))
    (unless (equal (alexandria:last-elt words) (end-sentence-token lm)) (setf words (append words (list (end-sentence-token lm)))))
    (setf words (substitute-oovs lm words))
    (loop for i from 2 to (length words) ;; this starts from 2 b/c variable i is tracking the *end* of the "history"
       for begin-history = (if (order lm)
			       (max 0 (- i (order lm))) ;; these should both be 0, the begin sent marker is in the history!!
			       0)
       for history = (subseq words begin-history i)
       for log-prob = (log-prob-of-history lm history) do
	 (incf log-prob-sum log-prob))
    log-prob-sum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; UNIFORM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* uniform-lm (conditional-lm)
  ((history-length 0)
   (order 1))
  (:documentation "This LM is a 'conditional' one, but all words are equally likely, and the history is ignored."))

(defmethod generate-word ((lm uniform-lm) history)
  "Generate a word uniformly."
  (declare (ignore history))
  (let* ((index (random (length (vocab lm))))
	 (word (aref (vocab lm) index)))
    word))

(defmethod prob-of-word ((lm uniform-lm) word history)
  "Get the uniform prob of a word, which = 1/vocab_size."
  (declare (ignore history))
  (assert (gethash word (vocab-table lm))) ;; Make sure the word is in the vocab.
  (/ 1.0 (vocab-size lm)))

(defmethod prob-of-history ((lm uniform-lm) history)
  (prob-of-word lm (first (last history)) nil))

(defmethod log-prob-of-history ((lm uniform-lm) history)
  (log-prob-of-word lm (first (last history)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Instantiable conditional models  -- Ngram and uniform ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass* ngram-lm (conditional-lm)
  () (:documentation "A conditional LM where the prob of a word depends only on the previous n words."))

(defgeneric log-3gram-prob (ngram-lm 3-gram)
  (:documentation "Get a probability for the 3-gram.  Back-off if necessary."))

(defgeneric log-2gram-prob (ngram-lm 2-gram)
  (:documentation "Get a probability for the 2-gram.  Back-off if necessary."))

;;; These are NOT implemented at this level, and must be implemented by subclasses
(defgeneric log-1gram-prob (ngram-lm 1-gram)
  (:documentation "Get a probability for the 1-gram."))

(defgeneric log-2gram-prob-internal (ngram-lm 2-gram)
  (:documentation "Get a probability for the 2-gram.  *Don't* back off."))

(defgeneric log-3gram-prob-internal (ngram-lm 3-gram)
  (:documentation "Get a probability for the 3-gram.  *Don't* back off."))

(defgeneric 1gram-bow-internal (ngram-lm 1-gram)
  (:documentation "Get the bow for the 1-gram."))

(defgeneric 2gram-bow-internal (ngram-lm 2-gram)
  (:documentation "Get the bow for the 2-gram."))

;;; Implemented methods
(defmethod generate-word ((lm ngram-lm) history)
  "Randomly generate a word given the history."
  (sample-ngram (history-length lm) history lm))

;; This is consing here...
(defmethod log-prob-of-word ((lm ngram-lm) word history)
  "Wrapper method for log-prob-history in this case."
  (declare (optimize (speed 3)))
  (log-prob-of-history lm (append history (list word))))

(defmethod log-prob-of-history ((lm ngram-lm) history)
  "The method that actually gets the n-gram probability, by calling out to the code in ngram.lisp."
  (declare (optimize (speed 3)))
  (setf history (substitute-oovs lm history))  ;; replace OOVs with the OOV token here
  (let ((log-prob (ngram-lm-log-prob history lm)))
    (if (= 10 (lm-log-base lm))
	log-prob
	(convert-log-prob-from-base-x-to-y log-prob 10 (lm-log-base lm)))))

(defmethod generate-word ((lm ngram-lm) history)
  "Randomly generate a word given the history."
  (sample-ngram (history-length lm) history lm))

(defmethod log-2gram-prob ((lm ngram-lm) 2-gram)
  "Get a probability for the 2-gram.  Back-off if necessary."
  (let ((p (log-2gram-prob-internal lm 2-gram)))
    (when p
      (return-from log-2gram-prob p)))
  (let ((backoff-weight (1gram-bow-internal lm (subseq 2-gram 0 1)))
	(unigram-prob (log-1gram-prob lm (subseq 2-gram 1 2))))
    (unless backoff-weight
      (setf backoff-weight 0.0))
    (unless unigram-prob
      (setf unigram-prob 0.0))
    (+ backoff-weight unigram-prob)))

(defmethod log-3gram-prob ((lm ngram-lm) 3-gram)
  "Get a probability for the 3-gram.  Back-off if necessary."
  (let ((p (log-3gram-prob-internal lm 3-gram)))
    (when p
      (return-from log-3gram-prob p)))
  (let ((backoff-weight (2gram-bow-internal lm (subseq 3-gram 0 2)))
	(log-bigram-prob (log-2gram-prob lm (subseq 3-gram 1 3))))
    (unless backoff-weight
      (setf backoff-weight 0.0))
    (+ backoff-weight log-bigram-prob)))

(defmethod log-prob-of-word ((lm ngram-lm) word history)
  "Log probability of next word given a list of words before it."
  (log-prob-of-history lm (append history (list word))))

(defmethod log-prob-of-history ((lm ngram-lm) history)
  (setf history (subseq history (max 0 (- (length history) (order lm))) (length history)))
  (let ((p (cond ((= (length history) 3)
		  (log-3gram-prob lm history))
		 ((= (length history) 2)
		  (log-2gram-prob lm history))
		 ((= (length history) 1)
		  (log-1gram-prob lm history))
		 (t (error "OOV!")))))
    (assert p)
    p))

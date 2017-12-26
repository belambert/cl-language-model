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

(defparameter *dependency-extension* ".parsed.gz" 
  "The name of an nbest list after POS tagging and dependency parsing.")

(defclass evaluable ()
  () (:documentation   "This is something we can compute a WER for... in particular a sentence or n-best list."))

(defclass* sentence-sparse (evaluable)
  ((total-score nil ira "" :type (or null single-float))
   (model-score 0.0d0 ira "" :type double-float)
   (combined-score nil ira "" :type (or null double-float))
   ;; More compact 'features' of the sentence
   (features nil ira "" :type '(or null (simple-array fixnum)))                       ;;; this is an array of fixnums.
   (active-features nil ira "" :type '(or null (simple-array fixnum)))                ;;; this is an array of fixnums.
   (evaluation nil ira))
  (:automatic-accessors t)
  (:name-prefix "sentence" "-")
  (:automatic-initargs t)
  (:documentation "A *sparse* representation of a 'sentence' or utterance, possibly ASR output or from a ref transcript."))

(defun sentence->sentence-sparse (s)
  "This creates a new instance of a sparse sentence, copying sparse values."
  (make-instance 'sentence-sparse
		 :total-score (sentence-total-score s)
		 :model-score (sentence-model-score s)
		 ;;:acoustic-score (sentence-acoustic-score s)
		 :combined-score (sentence-combined-score s)
		 :features (sentence-features s)
		 :evaluation (sentence-evaluation s)))

(defclass* sentence (sentence-sparse)
  ((example-id nil ira) ;; rename this to something like "id"?
   ;; ASR-related scores
   ;;(log-base 1.0001 ira)
   (lm-score nil);; ira ""  :type (or null number))
   ;; the actual words, represented in several ways
   (words nil) ;; ira "") ;; ("ONE" "SOUL" "TO" "MORNING" "LAST" "JUNE"
   pos-tags
   (segmented-words nil)
   (dependencies nil ira)   
   ;; Should we keep these separate?
   (patterns nil ira)
   (lexical-patterns nil ira)
   (non-lexical-patterns nil ira))
  (:automatic-accessors t)
  (:name-prefix "sentence" "-")
  (:automatic-initargs t)
  (:documentation "Represents a 'sentence' or utterance, possiblye ASR output or from a ref transcript."))

(defun read-sentences (filename &key conll-filename)
  "Read a file the contains one transcription per line, followed by an ID in parens, into a list of lists."
  (let* ((sentences '())
	 (dependency-filename (if conll-filename
				  conll-filename
				  (concatenate 'string (base-filename filename) *dependency-extension*)))
	 (dependencies (when (probe-file dependency-filename)
			 (read-conll-file dependency-filename))))
    (do-lines (line filename)
      (unless (string-equal line "")
	(let* ((str-length (length line))
	       (begin-id (position #\( line :from-end t))
	       (example-id (when begin-id (subseq line (1+ begin-id) (1- str-length))))
	       (transcript (subseq line 0 (or begin-id (length line))))
	       ;; The token boundaries are determined by spaces...
	       (tokens (split-sequence:split-sequence #\Space transcript :remove-empty-subseqs t)))
	  (map-into tokens 'get-cached-string tokens)
	  (alexandria:coercef tokens 'vector)
	  ;;(setf example-id (get-cached-string example-id))
	  (push
	   (make-instance 'sentence :words tokens :example-id example-id)
	   sentences))))
    (setf sentences (nreverse sentences))
    (mapc 'copy-dependency-to-sentence dependencies sentences)
    sentences))

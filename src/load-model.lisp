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
;;;;;;; LM I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-model (filename &key type vocab feature-gain)
  "Load a model from a file.  Since we don't have a CLOS object a priori, this is just a regular function.
   If the type is not specified, then it is interpreted from the file extension (.ngram, .cme, or .wsm)."
  (unless type
    (let* ((filename-base (if (alexandria:ends-with-subseq ".gz" filename)
			      (subseq filename 0 (- (length filename) 3))
			      filename))
	   (ext-begin (position #\. filename-base :from-end t))
	   (file-ext (subseq filename-base (1+ ext-begin))))
      (cond ((string-equal file-ext "ngram")
	     (setf type :ngram))
	    ((string-equal file-ext "lm")
	     (setf type :ngram-arpa))
	    ((string-equal file-ext "dmp")
	     (setf type :ngram-dmp))
	    ((string-equal file-ext "simple")
	     (setf type :pattern))
	    ((string-equal file-ext "model")
	     (setf type :pattern)))))
  (let ((model (case type
		 (:ngram-arpa (make-instance 'ngram-lm-arpa :filename filename))
		 (:ngram-dmp (ngram-model-dmp-read filename))
		 (:pattern (load-pattern-lm filename))
		 (t (error "~A is not a known LM type!" type)))))
    (when vocab
      (if (stringp vocab)
	  (setf (vocab model) (load-vocab vocab))
	  (setf (vocab model) vocab)))
    (force-output t)
    model))

(defun array->id-table (array ht)
  "Given an array and a hashtable, turn the hashtable into an index of the array indicies."
  (loop for i from 0 below (length array)
     for elt = (aref array i) do
       (setf (gethash elt ht) i)))

(defun populate-hash-table-with-array-indicies (array ht)
  "Given an array and a hashtable, turn the hashtable into an index of the array indicies."
  (array->id-table array ht))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SIMPLE LM I/O   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-pattern-lm (filename)
  "Load a 'simple' LM."
  (let* ((model-list (load-object filename))
	 (parameters (getf model-list :parameters)))
    (setf (getf model-list :parameters)
	  (make-array (length parameters)  ;; the array has to have the correct type, for efficiency
		      :element-type 'double-float
		      :initial-contents parameters))
    (let ((model (apply #'cl-user::make-instance 'pattern-lm model-list)))
      (format t "Vocab size:    ~:D~%" (length (vocab model)))
      (format t "Feature count: ~:D~%" (length (parameters model)))
      model)))

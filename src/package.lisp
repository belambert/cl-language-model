;;;; Ben Lambert (ben@benjaminlambert.com)

(defpackage :language-model
  (:nicknames :lm)
  (:use :common-lisp :alexandria :split-sequence)
  (:import-from :metatilities :defclass* :defclass-brief)
  (:export :score
	   :parameters
	   :pattern-modes
	   :pattern-file
	   :patterns
	   :pattern-table
	   :non-lexical-patterns
	   :parameters
	   :score-combination-type
	   :contrast-as-weight-p
	   :contrast-multiplier
	   :abstraction-functions
	   :grammar-file
	   :filler-file	   
	   :log-prob-of-sentence))






;; -*- Mode: Lisp -*-

(asdf:defsystem "cl-lm"
  :description "Common Lisp language modeling"
  :version "0.1.0"
  :author "Ben Lambert"
  :serial t
  :components
  ((:module src
	    :serial t
	    :components ((:file "package")
			 (:file "sentence")			 
			 (:file "clos-lm")
			 (:file "load-model")
			 (:file "ngram")
			 (:file "ngram-model-dmp")
			 (:file "interpolating-lms")
			 (:file "perplexity")
			 (:file "vocab")
			 (:file "vocab-io")
			 (:file "util"))))
  :depends-on (:cl-ppcre
	       :split-sequence
	       :alexandria
	       :array-operations
	       :cl-fad
	       :metatilities
	       :ieee-floats
	       :parse-number
	       :gzip-stream
	       :gnuplot))


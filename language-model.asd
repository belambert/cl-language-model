;;-*- Mode: Lisp -*-
;;;; Benjamin E. Lambert (ben@benjaminlambert.com)

(asdf:defsystem "language-model"
  :description "Common Lisp langauge modeling"
  :version "0.1.0"
  :author "Benjamin Lambert"
  :licence "All rights reserved"
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
			 (:file "vocab-io"))))
  :depends-on (:lispdoc :blambert-util
			:cl-ppcre
			:split-sequence
			:alexandria
			:array-operations
			:cl-fad
			:metatilities
			:ieee-floats
			;;:pos-tagger
			))


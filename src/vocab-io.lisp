;;;; Benjamin E. Lambert (ben@benjaminlambert.com)

(declaim (optimize (debug 3)))
(in-package :language-model)
(cl-user::file-summary "Input and output of language models and vocabularies")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; VOCAB   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-user::section "Vocab I/O")

(cl-user::TODO "let's make this consistent with the SRI format...")

(defun load-vocab (filename)
  "Load a vocabulary file, returning it as an array..."
  (let ((vocab (blambert-util::load-object filename)))
    (make-array (length vocab) :element-type 'string :initial-contents vocab)))

(defun save-vocab (vocab filename)
  "Save a vocabulary (array?) to the specified file."
  (blambert-util::save-object vocab filename))


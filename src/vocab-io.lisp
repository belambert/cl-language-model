;;;; Ben Lambert (ben@benjaminlambert.com)

(in-package :language-model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; VOCAB   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO let's make this consistent with the SRI format

(defun load-vocab (filename)
  "Load a vocabulary file, returning it as an array..."
  (let ((vocab (load-object filename)))
    (make-array (length vocab) :element-type 'string :initial-contents vocab)))

(defun save-vocab (vocab filename)
  "Save a vocabulary (array?) to the specified file."
  (save-object vocab filename))


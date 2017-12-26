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

(defun hash-table->list (ht)
  "Convert a hashtable to a list of key value pairs."
  (let ((list '()))
    (maphash (lambda (x y) (push (list x y) list)) ht)
    list))

(defun save-object (object filename)
  "Given an object print it to the given filename."
  (ensure-directories-exist filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (prin1 object stream)))

(defun load-object (filename)
  "Read an object from a file."
  (if (alexandria:ends-with-subseq ".gz" filename :test 'char-equal)
      (gzip-stream:with-open-gzip-file (stream filename :direction :input)
	(setf stream (flexi-streams:make-flexi-stream stream))
	(read stream nil))
      (with-open-file (stream filename :direction :input)
	(read stream nil))))

(defmacro do-lines ((var filename &key (external-format :utf-8)) &body body)
  "Macro that iterates through the lines of a file, binding the line (as a string)
   to the variable name specified.  Reads the entire file into memory quickly up-front."
  (declare (optimize (speed 3)))
  `(let ((line-count 0)
	 (lines (file->line-list ,filename :external-format ,external-format)))
     (declare (fixnum line-count))
     (dolist (,var lines)
       (declare (simple-string ,var))
       (incf line-count)
       ,@body)))

(defparameter *string-cache* (make-hash-table :test 'equal)
  "To save memory we can ensure that strings are not replicated in memory.  This table stores the 'shared' version of the string.")

(defun get-cached-string (s)
  "Get the global, shared version of the given string.  If the string isn't already cached, use the given string."
  (let ((cached (gethash s *string-cache*)))
    (if cached
	cached
	(progn
	  (setf (gethash s *string-cache*) s)
	  s))))

(defun convert-log-prob-from-base-x-to-y (log-prob x y)
  "Convert a log-prob from one base to another.  Mostly used to convert from ARPA base 10 to our base 2 standard."
  (declare (fixnum x y)
	   (number log-prob))
  (let ((divisor (log y x)))
    (declare (float divisor))
    (/ log-prob divisor)))

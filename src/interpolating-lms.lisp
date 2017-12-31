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

(in-package :cl-lm)

(defvar *default-interpolation-epsilon* 1e-3
  "Default epsilon value for when EM should be consider to be converged, and we should stop.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Degenerate-EM alg for interpolation ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun likelihood (i)
  "Get the likelihood using the current lambda values."
  (let ((lambda1 (get-lambda-n 0 i))
	(lambda2 (get-lambda-n 1 i))
	(length (length (get-stream-n 0 i))))
    (declare (single-float lambda1 lambda2)
	     (fixnum length))
  (/ (reduce '+
	     (map 'vector (lambda (x) (log x 2))
		  (map 'vector (lambda (x y) (declare (single-float x y)) (+ x y))
		       (map 'vector (lambda (x) (* (the single-float x) lambda1))
			    (get-stream-n 0 i))
		       (map 'vector (lambda (x) (* (the single-float x) lambda2))
			    (get-stream-n 1 i)))))
     length)))
   
(defun next-parameter-value (interp j)
  "Compute the next value for lambda_j (?)."
  (let ((sum 0)
	(stream-length (length (get-stream-n j interp))))
    (dotimes (i stream-length)
      (incf sum
	    (/ (* (get-lambda-n j interp)
		  (elt (get-stream-n j interp) i))
	       (+ (* (get-lambda-n 0 interp)
		     (elt (get-stream-n 0 interp) i))
		  (* (get-lambda-n 1 interp)
		     (elt (get-stream-n 1 interp) i))))))
    (/ sum stream-length)))

(defun degenerate-em (i &key (epsilon *default-interpolation-epsilon*))
  "Main loop for the interpolation"
  (let ((previous-average-log-likelihood nil)
	(iteration-num 0))
    (declare ((or null single-float) previous-average-log-likelihood))
    (loop until nil do
	 (let ((this-likelihood (likelihood i)));; get the current likelihood
	   (declare (single-float this-likelihood))
	   (when (= (mod iteration-num 1) 0)
	     (format t "~2A. Lambdas: ~8,8f, ~8,8f; LL: ~8,8f (change: ~8,8f)~%"
		     iteration-num
		     (get-lambda-n 0 i)
		     (get-lambda-n 1 i)
		     this-likelihood
		     (if previous-average-log-likelihood
			 (- this-likelihood
			    previous-average-log-likelihood)
			 nil))
	     (force-output t))
	   (incf iteration-num)
	   ;; update the lambdas
	   (setf (elt (interpolation-lambdas i) 0) (next-parameter-value i 0))
	   (setf (elt (interpolation-lambdas i) 1) (next-parameter-value i 1))
	   ;; check for convergence
	   (when (and previous-average-log-likelihood
		      (< (abs (- this-likelihood previous-average-log-likelihood))
			 epsilon))
	     (format t "FINISHED.~%")
	     (format t "FINAL LAMBDA VALUES: ~10F  ~10F~%" (get-lambda-n 0 i) (get-lambda-n 1 i))
	     (return))
	   (setf previous-average-log-likelihood this-likelihood))))
  (list (get-lambda-n 0 i) (get-lambda-n 1 i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Main linear interpolation functions ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpolate-two-prob-streams (s1 s2 interpolation-weights)
  "Given two probability streams, and some interpolation weights, create a new linearly
   interpolated probability stream."
  ;;(declare (optimize (speed 3))
  ;;         ((simple-array single-float) s1 s2))
  (let* ((lambda1 (first interpolation-weights))
	 (lambda2 (second interpolation-weights))
	 (interpolated-probs (map 'vector (lambda (x y) (declare (single-float x y)) (+ x y))
				  (map 'vector (lambda (x) (* lambda1 (the single-float x))) s1)
				  (map 'vector (lambda (x) (* lambda2 (the single-float x))) s2))))
    (declare (single-float lambda1 lambda2))
    interpolated-probs))


(defstruct interpolation
  "A partial prob stream interpolation."
  streams
  lambdas)

(defun get-stream-n (n i)
  "Get the n'th probability stream from partial interpolation i."
  (elt (interpolation-streams i) n))

(defun get-lambda-n (n i)
  "Get the n'th lambda from partial interpolation i."
  (elt (interpolation-lambdas i) n))

(defun get-initial-interpolation-struct (stream-list)
  "Create an initial partial interpolation from the given stream list.  Initializes lambdas to 1 / (# of streams)."
  (let* ((length (length stream-list))
	 (init-lambda (/ 1.0 length)))
    (make-interpolation :streams (make-array length :initial-contents stream-list)
			:lambdas (make-array length :initial-element init-lambda))))
     
(defun interpolate (s1 s2 &key (epsilon *default-interpolation-epsilon*))
  "Given two probability stream.  Use EM to compute an optimal set of lambda weights.
   Then interopolate the streams, print some stats, and return the lambda values."
  (setf s1 (coerce s1 'vector))
  (setf s2 (coerce s2 'vector))
  (let* ((init-interpol (get-initial-interpolation-struct (list s1 s2)))
	 (lambda-list (degenerate-em init-interpol :epsilon epsilon))
	 (interpolated-probs (interpolate-two-prob-streams s1 s2 lambda-list)))
    (format t "LAMBDA VALUES: ~{~10f  ~}~%" lambda-list)
    (format t "S1 PPL:~%")
    (print-prob-stream-ppl s1)
    (format t "S2 PPL:~%")
    (print-prob-stream-ppl s2)
    (format t "INTERPOLATED PPL:~%")
    (print-prob-stream-ppl interpolated-probs)
    lambda-list))

(defun interpolate-two-lms (lm1 lm2 data &key (epsilon *default-interpolation-epsilon*))
  "Given two models, derive probability stream, compute an optimal linear combination.
   Print something or other and return the interpolation weights."
  (let* ((prob-stream1 (lm-example-prob-stream data lm1))
	 (prob-stream2 (lm-example-prob-stream data lm2))
	 (interpolation-weights (interpolate prob-stream1 prob-stream2 :epsilon epsilon)))
    interpolation-weights))

(defun print-interpolated-ppl (lm1 lm2 data interpolation-weights)
  "Given two models, data, and some interpolation weights, print the interpolated PPL."
  (let* ((ps1 (lm-example-prob-stream data lm1))
	 (ps2 (lm-example-prob-stream data lm2))
	 (interpolated-prob-stream (interpolate-two-prob-streams ps1 ps2 interpolation-weights)))
    (format t "S1 PPL:~%")
    (print-prob-stream-ppl ps1)
    (format t "S2 PPL:~%")
    (print-prob-stream-ppl ps2)
    (format t "INTERPOLATED PPL:~%")
    (print-prob-stream-ppl interpolated-prob-stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; MaxEnt interpolation of LMs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; TODO "MaxEnt interpolation of LM.... this allows one LM to have 'negative' weight.  This is not even close to working
;;; TODO "See paper: Log-linear interpolation of language models, by Dietrich Klakow, ~1998


(defvar *normalization-table* (make-hash-table :test 'equalp)
  "The normalization depends on the specific history....?")

;; This is still using the lm-example struct... so I'm commenting it out until we fix it.

#|
(defun me-interpolate-two-lms (lm1 lm2 data lambda1 lambda2)
  "Given two LMs, some data, and two lambda values, compute and return the MaxEnt interpolated prob stream."
  (let ((interpolated-log-prob-stream '()))
    (dolist (lm-example data)
      (let* ((lm1-log-prob (log-prob-of-lm-example lm1 lm-example))
	     (lm2-log-prob (log-prob-of-lm-example lm2 lm-example))
	     (normalization-factor (gethash (lm-example-history lm-example) *normalization-table*))
	     ;; eq: -Z(h) + SUM (lambda_i * logP(w|h)
	     (interpolated-log-prob (+ (- normalization-factor) (* lambda1 lm1-log-prob) (* lambda2 lm2-log-prob))))
	(push interpolated-log-prob interpolated-log-prob-stream)))
    (nreverse interpolated-log-prob-stream)))

(defun print-me-interpolated-ppl (lm1 lm2 data interpolation-weights)
  "Given two models, data, and some interpolation weights, print the ME-interpolated PPL."
  (let* ((ps1 (lm-example-prob-stream data lm1))
	 (ps2 (lm-example-prob-stream data lm2))
	 (interpolated-prob-stream (me-interpolate-two-lms lm1 lm2 data (first interpolation-weights) (second interpolation-weights))))
    (format t "S1 PPL:~%")
    (print-prob-stream-ppl ps1)
    (format t "S2 PPL:~%")
    (print-prob-stream-ppl ps2)
    (format t "INTERPOLATED PPL:~%")
    (print-prob-stream-ppl interpolated-prob-stream)))
	

;;; TODO Z depends on the history, so we have to look at all histories, and make sure the next word probabilities sum to one...  ugh.")

(defun get-normalization-score (lm-example p1 p2 lambda1 lambda2)
  "????"
  (let ((cached (gethash (lm-example-history lm-example) *normalization-table*)))
    (if cached
	cached
	(progn
	  (let ((term1 (expt p1 lambda1))
		(term2 (expt p2 lambda2)))
	    (declare (ignore term1 term2))
	    ())))))
|#

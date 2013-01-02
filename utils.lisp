;; Copyright (c) 2012, Jason R. Person
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met: 
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer. 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution. 
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies, 
;; either expressed or implied, of the FreeBSD Project.

(in-package #:cl-poker-sim)

(defun flatten (x)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defun ashuffle (s)
  "Shuffles sequence s"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array fixnum (*)) s))
  (let ((len (length s)))
    (declare (type fixnum len))
    (loop for i of-type fixnum from len downto 2
          do (rotatef (aref s (random i))
                      (aref s (1- i)))))
   s)

(defun combination (m deck)
  "Generate combinations of length m for given deck"
  (let ((hands nil))
    (labels 
      ((combl (l c m)
         (when (>= (length l) m)
          (if (zerop m) (return-from combl (push c hands)))
            (combl (cdr l) c m)
            (combl (cdr l) (cons (first l) c) (1- m)))))
     (combl deck nil m))
    hands))

(defun atake (n l)
  "Returns first n elements of array l"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n) (type (simple-array fixnum (*)) l))
  (let ((r (make-array n :element-type 'fixnum)))
    (loop for i of-type fixnum from 0 upto (1- n)
          do (setf (aref r i) (aref l i)))
    r))

(defun alast (n l)
  "Returns last n elements of array l"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n) (type (simple-array fixnum (*)) l))
  (let ((r (make-array n :element-type 'fixnum))
        (len (1- (length l))))
    (loop for i of-type fixnum from len downto (- len n)
          as index from (1- n) downto 0
          do (setf (aref r index) (aref l i)))
    r))

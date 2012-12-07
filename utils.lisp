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
    (loop for i from len downto 2
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
    (loop for i from 0 upto (1- n)
          do (setf (aref r i) (aref l i)))
    r))

(defun alast (n l)
  "Returns last n elements of array l"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum n) (type (simple-array fixnum (*)) l))
  (let ((r (make-array n :element-type 'fixnum))
        (len (1- (length l))))
    (loop for i from len downto (- len n)
          as index from (1- n) downto 0
          do (setf (aref r index) (aref l i)))
    r))

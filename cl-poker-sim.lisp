(in-package #:cl-poker-sim)

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defun nshuffle (s)
  "Shuffles sequence s"
  (loop for i from (length s) downto 2
        do (rotatef (elt s (random i))
                    (elt s (1- i))))
  s)

(defun ashuffle (s)
  "Shuffles sequence s"
  (let ((len (length s)))
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

(defun take (n l)
  (loop for e in l
        for i from 1 upto n
        collect e))

(defun atake (n l)
  (let ((r (make-array n :fill-pointer 0)))
    (loop for i from 0 upto (1- n)
         do (vector-push (aref l i) r))
    r))

(defun alast (n l)
  (let ((r (make-array n :fill-pointer 0))
        (len (1- (length l))))
    (loop for i from len downto (- len n)
          do (vector-push (aref l i) r))
    r))

(defun ppot (hole board)
  "Calculates the ppot++ for given hole and board"
  (let* ((cards (loop for c from 0 upto 51 collect c))
         (deck (nset-difference cards (flatten (cons hole board)))))
    (ppot++ hole board deck)))

(defun ppot- (hole board deck)
  "Calculates the positive potential for the current board only"
  (let ((hval (apply #'cl-poker-eval:eval-hand-var (flatten (cons hole board))))
        (oph (combination 2 deck))
        (pp 0) (np 0) (ep 0))
    (labels ((ppot1 (oh)
               (let ((oval (apply #'cl-poker-eval:eval-hand-var (flatten (cons board oh)))))
                (cond
                    ((< hval oval) (incf np))
                    ((> hval oval) (incf pp))
                    ((= hval oval) (incf ep))))))
      (loop for h in oph do (ppot1 h)))
    (values pp ep np)))
         
(defun ppot+ (hole board deck)
  "Calculates the positive potential for next card only"
  (let ((pp 0) (np 0) (ep 0))
     (loop for c in deck do
      (multiple-value-bind (pp2 ep2 np2) 
        (ppot- hole (flatten (cons board c)) (set-difference deck (list c)))
        (setf pp (+ pp pp2))
        (setf np (+ np np2))
        (setf ep (+ ep ep2))))
    (values pp ep np)))

(defun ppot++ (hole board deck)
  "Calculates the positive potenital for turn+river"
  (let ((pp 0) (np 0) (ep 0))
     (loop for cs in (combination 2 deck) do
      (multiple-value-bind (pp2 ep2 np2) 
        (ppot- hole (flatten (cons board cs)) (set-difference deck cs))
        (setf pp (+ pp pp2))
        (setf np (+ np np2))
        (setf ep (+ ep ep2))))
    (values pp ep np)))

(defun sim (hole &rest ophs)
  "Calculates win/tie/lose for given hole cards against given opponent hold cards"
  (let* ((cards (loop for c from 0 upto 51 collect c))
        (deck (set-difference cards (flatten (cons hole ophs))))
        (boards (combination 5 deck))
        (win 0) (lose 0) (tie 0))
    (loop for h in boards do
         (let* ((hval (apply #'cl-poker-eval:eval-hand-var (flatten (cons h hole))))
               (opvals (map 'list #'(lambda (oh) (apply #'cl-poker-eval:eval-hand-var (flatten (cons oh h)))) ophs))
               (maxoh (apply #'max opvals)))
           (cond 
             ((< hval maxoh) (incf lose))
             ((> hval maxoh) (incf win))
             ((= hval maxoh) (incf tie)))))
  (values win tie lose)))

(defun monte-carlo-sim (n hole noppts)
  "Monte-carlo sim of n hands using hole cards against noppts oppoents"
  ;(declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (let* ((cards (make-array 50 :initial-contents (nset-difference (loop for c from 0 upto 51 collect c) hole)))
         (pp 0) (np 0) (ep 0))
    (loop for i from 1 upto n do
          (let* ((deck (ashuffle cards))
                 (board (coerce (atake 5 deck) 'list))
                 (opsdeck (alast 45 deck))
                 (ophs (map 'list #'list (atake noppts opsdeck) (alast noppts opsdeck)))
                 (hval (apply #'cl-poker-eval:eval-hand-var (flatten (cons board hole))))
                 (opvals (map 'list #'(lambda (oh) (apply #'cl-poker-eval:eval-hand-var (flatten (cons board oh)))) ophs)) (maxop (apply #'max opvals)))
            (cond
              ((< hval maxop) (incf np))
              ((> hval maxop) (incf pp))
              ((= hval maxop) (incf ep)))))
    (values pp ep np)))


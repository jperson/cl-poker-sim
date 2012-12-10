(in-package #:cl-poker-sim)

(defun ppot (hole board)
  "Calculates the ppot++ for given hole and board"
  (let* ((cards (loop for c of-type fixnum from 0 upto 51 collect c))
         (deck (nset-difference cards (flatten (cons hole board)))))
    (ppot++ hole board deck)))


(defun sim (hole &rest ophs)
  "Calculates win/tie/lose for given hole cards against given opponent hold cards"
  (let* ((cards (loop for c of-type fixnum from 0 upto 51 collect c))
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
  "Monte-carlo sim of n hands using hole cards against noppts opponents"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
           (type fixnum n noppts))
  (let* ((cards (make-array 50 :element-type 'fixnum :initial-contents (nset-difference (loop for c of-type fixnum from 0 upto 51 collect c) hole)))
         (pp 0) (np 0) (ep 0) (tp 0))
    (loop for i of-type fixnum from 1 upto n do
          (let* ((deck (ashuffle cards))
                 (board (coerce (atake 5 deck) 'list))
                 (opsdeck (alast 45 deck))
                 (ophs (loop for x across (atake noppts opsdeck)
                             for y across (alast noppts opsdeck) collect (list x y)))
                 (hval (apply #'cl-poker-eval:eval-hand-var (flatten (cons board hole))))
                 (opvals (loop for oh in ophs collect (apply #'cl-poker-eval:eval-hand-var (flatten (cons board oh)))))
                 (maxop (apply #'max opvals)))
            (declare (type fixnum maxop hval np pp ep))
            (cond
              ((< hval maxop) (incf np))
              ((> hval maxop) (incf pp))
              ((= hval maxop) (incf ep)))))
    (setf tp (+ pp ep np))
    (values 
      (float (/ pp tp)) 
      (float (/ ep tp))
      (float (/ np tp)))))

(defun noppts-sim (n hole board noppts)
  "Monte-carlo sim of n hands using hole cards and board against noppts opponents"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0))
                     (type fixnum n noppts))
  (let* ((cards (make-array 47 :element-type 'fixnum :initial-contents (nset-difference (loop for c of-type fixnum from 0 upto 51 collect c) (flatten (cons board hole)))))
         (hval (apply #'cl-poker-eval:eval-hand-var (flatten (cons board hole))))
         (pp 0) (np 0) (ep 0) (tp 0))
    (loop for i of-type fixnum from 1 upto n do
          (let* ((opsdeck (ashuffle cards))
                 (ophs (loop for x across (atake noppts opsdeck)
                             for y across (alast noppts opsdeck) collect (list x y)))
                 (opvals (loop for oh in ophs collect (apply #'cl-poker-eval:eval-hand-var (flatten (cons board oh)))))
                 (maxop (apply #'max opvals)))
            (declare (type fixnum maxop hval np pp ep))
            (cond
              ((< hval maxop) (incf np))
              ((> hval maxop) (incf pp))
              ((= hval maxop) (incf ep)))))
    (setf tp (+ pp ep np))
    (values
      (float (/ pp tp))
      (float (/ ep tp))
      (float (/ np tp)))))


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
     (loop for c of-type fixnum in deck do
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


;;; (load "simple-graph.lisp")

(defun space-over (N)
  (cond ((< N 0) (format t "Error!"))
        ((= N 0) nil)
        (t (or (format t " ")
               (space-over (- N 1))))))

(defun test-space-over (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

;;; (test-space-over 5)
;;; (test-space-over -5)


(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t plotting-string)
  (format t "~&"))


(defun test-plot-one-point ()
  (plot-one-point "x" 0)
  (plot-one-point "x" 1)
  (plot-one-point "x" 2)
  (plot-one-point "x" 1)
  (plot-one-point "x" 0)
  (plot-one-point "****" 0)
  (plot-one-point "***" 1)
  (plot-one-point "**" 2)
  (plot-one-point "*" 3))


(defun plot-points (plotting-string points)
  (cond ((null points) nil)
        (t (or (plot-one-point plotting-string (car points))
               (plot-points plotting-string (cdr points))))))


(defun test-plot-points  ()
  (plot-points "<>" '(4 6 8 10 8 6 4))
  (plot-points "*" '(0 1 2 3 2 1 0)))


;;; Makes a list of ints from M to N, inclusive.
;;; (generate -3 3) -> (-3 -2 ... 2 3)
(defun generate (M N)
  (cond ((> M N) (format t "Error: M > N"))
        ((= M N) (list N))
        (t (cons M (generate (+ M 1) N)))))


(defun make-graph ()
  (let ((func (or (format t "~&Enter a function: ")
                  (read)))
        (start (or (format t "~&Enter a starting point: ")
                   (read)))
        (end (or (format t "~&Enter an ending point: ")
                 (read)))
        (plotting-string (or (format t "~&Enter a plot symbol: ")
                             (read))))
    (plot-points plotting-string (mapcar func (generate start end)))))


;;; Test methods
(defun square (x)
  (* x x))

(defun abs-cube (x)
  (abs (* x x x)))

(defun line (x m b)
  (+ (* m x) b))

(defun line-1-1 (x)
  (line x 1 1))



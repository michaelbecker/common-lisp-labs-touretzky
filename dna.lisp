;;; DNA Example

(defun COMPLEMENT-BASE (base)
  (cond ((equal base 'T) 'A)
	((equal base 'A) 'T)
	((equal base 'G) 'C)
	((equal base 'C) 'G)
	(t (format t "Unknown base ~S~%" base) nil)))

;;; Test 
(defun test1 ()
  (format t "A -> ~S~%" (COMPLEMENT-BASE 'A))
  (format t "T -> ~S~%" (COMPLEMENT-BASE 'T))
  (format t "C -> ~S~%" (COMPLEMENT-BASE 'C))
  (format t "G -> ~S~%" (COMPLEMENT-BASE 'G))
  (format t "X -> ~S~%" (COMPLEMENT-BASE 'X))
  (format t "Test Complete"))

(defun COMPLEMENT-STRAND (strand)
  (let (( complement () ))
    (dolist (base strand)
      (push (COMPLEMENT-BASE base) complement))
    complement))

(defun test2 ()
  (let* ((strand '(A T C G G C T A))
	(complement (COMPLEMENT-STRAND strand)))
    (format t "test2: ~S -> ~S~%" strand complement)))


(defun MAKE-DOUBLE (strand)
  (let ((complement (COMPLEMENT-STRAND strand))
	(dna ()))
    (do ((x (pop strand) (pop strand))
	 (y (pop complement) (pop complement)))
	((null x) (reverse dna))
	(push (list x y) dna))))

(defun test3 ()
  (let* ((strand '(A T T G G C))
	 (dna (MAKE-DOUBLE strand)))
    (format t "test3: ~S -> ~S~%" strand dna)))


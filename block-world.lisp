;;; To run in clisp:
;;; (load "filename")

(defvar *database*)

(setf *database* 
      '((b1 shape brick)
        (b1 color green)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)

        (b2 shape brick)
        (b2 color red)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)

        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of b2)

        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)

        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)

        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))


(defun match-element (e1 e2)
  (cond ((equal '? e2) t)
        ((equal e1 e2) t)
        ( t nil)))

;;; Test 
;;; (match-element 'red 'red) 
;;; (match-element 'red '?)
;;; (match-element 'red 'blue)


(defun match-triple (assertion pattern)
  (every #'match-element assertion pattern))

;;; Test
;;; (match-triple '(b2 color red) '(b2 color ?))
;;; (match-triple '(b2 color red) '(b2 color green))


(defun fetch (pattern)
    (remove-if-not #'(lambda (dbentry)
                       (match-triple dbentry pattern))
                   *database*))

;;; Test
;;; (fetch '(b2 color ?))
;;; (fetch '(? supports b1))

(defun color-pattern (block-name)
  (list block-name 'color '?))

(defun supporters (block-name)
  (mapcar #'caddr (fetch (list block-name 'supported-by '?))))


(defun supp-shape (block-name shape)
  (let ((supp-blocks (supporters block-name))
        (shape-entries (mapcar #'car (fetch (list '? 'shape shape)))))
    (intersection supp-blocks shape-entries)))

(defun supp-cube (block-name)
  (supp-shape block-name 'cube))

(defun desc1 (block-name)
  (fetch (list block-name '? '?)))

(defun desc2 (block-name)
  (mapcar #'cdr (desc1 block-name)))

(defun description (block-name)
  (reduce #'append (desc2 block-name)))



;;; (load "filename.lisp")

;;; (name father mother)
(defvar *family*)
(setf *family* 
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))


(defun father (p)
  (cadr (assoc p *family*)))

(defun mother (p)
  (caddr (assoc p *family*)))

(defun parents (p)
  (remove-if #'null (cdr (assoc p *family*))))

(defun get-child (entry parent)
  ;; Given an entry (child father mother) and a parent, 
  ;; returns the child or nil.
  (let ((p1 (cadr entry))
        (p2 (caddr entry))
        (child (car entry)))
    (cond ((equal parent p1) child)
          ((equal parent p2) child)
          (t nil))))

(defun children (parent)
  (remove-if #'null 
    (mapcar #'(lambda (entry) (get-child entry parent))
            *family*)))


;;; (siblings 'bruce) -> (charles david ellen)
;;; (siblings zelda)  -> (joshua)
(defun siblings (child)
  (let ((p1 (car (parents child)))
        (p2 (cadr (parents child))))
    (set-difference (union (children p1) (children p2))
                    (list child))))


(defun mapunion (fcn lst)
  (reduce #'union (mapcar fcn lst)))

(defun grandparents (child)
  (mapunion #'parents (parents child)))

;;; julie -> tamara vincent nigel
(defun cousins (child)
  (mapunion #'children (mapunion #'siblings (parents child))))


;;; tamara arthur -> t
;;; tamara linda -> nil
(defun descended-from (child ancestor)
  (let ((p1 (mother child))
        (p2 (father child)))
    (cond ((null child) nil)
          ((null ancestor) nil)
          ((equal ancestor p1) t)
          ((equal ancestor p2) t)
          (t (or (descended-from p1 ancestor)
                 (descended-from p2 ancestor))))))


;;; (ancestors 'marie) -> (GEORGE ELLEN ARTHUR KATE FRANK LINDA)
;;; (ancestors 'kate) -> nil
(defun ancestors (person)
  (cond ((null person) nil)
        (t (append (parents person)
                  (ancestors (mother person))
                  (ancestors (father person))))))


(defun generation-gap-recurse-count (person ancestor iter)
  (cond ((null person) 0)
        ((member ancestor (parents person)) iter)
        (t (max (generation-gap-recurse-count (mother person) ancestor (+ iter 1))
              (generation-gap-recurse-count (father person) ancestor (+ iter 1))))))

;;; (generation-gap 'suzanne 'colin) -> 1   // Parent
;;; (generation-gap 'frederick 'colin) -> 3 // great grandparent
;;; (generation-gap 'frederick 'linda) -> nil
;;; (generation-gap 'olivia 'frank) -> 3
(defun generation-gap (person ancestor)
  (let ((cnt (generation-gap-recurse-count person ancestor 1)))
    (if (equal cnt 0) nil cnt)))



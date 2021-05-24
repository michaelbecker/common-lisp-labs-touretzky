;;; To run in clisp:
;;; (load "filename")

(defvar *rooms*)

(setf *rooms* 
  '((living-room    
      (north    front-stairs)
      (south    dining-room)
      (east     kitchen))
    (upstairs-bedroom
      (west     library)
      (south    front-stairs))
    (dining-room
      (north    living-room)
      (east     pantry)
      (west     downstairs-bedroom))
    (kitchen
      (west     living-room)
      (south    pantry))
    (pantry
      (north    kitchen)
      (west     dining-room))
    (downstairs-bedroom
      (north    back-stairs)
      (east     dining-room))
    (back-stairs
      (south    downstairs-bedroom)
      (north    library))
    (front-stairs
      (north    upstairs-bedroom)
      (south    living-room))
    (library
      (east     upstairs-bedroom)
      (south    back-stairs))))

(defun choices (r)
  "Takes a room name and returns it's connections."
  (cdr (assoc r *rooms*)))
   
(defun look (dir r)
  "Returns where Robbie would end up if it went there."
  (cadr (assoc dir (choices r))))


(defvar *loc*)

(defun set-location (loc)
  (setf *loc* loc))

(defun how-many-choices ()
  (length (choices *loc*)))

(defun upstairsp (r)
  (cond ((equal r 'upstairs-bedroom) t)
        ((equal r 'library) t)
        (t nil)))

(defun onstairsp (r)
  (if (equal r 'front-stairs) t
    (if (equal r 'back-stairs) t nil)))

(defun where ()
  (let ((loc (car (assoc *loc* *rooms*))))
    (cond ((onstairsp loc) (list 'robbie 'is 'on 'the loc))
          ((upstairsp loc) (list 'robbie 'is 'upstairs 'in 'the loc))
          (t (list 'robbie 'is 'downstairs 'in 'the loc)))))

(defun move (dir)
  (let* ((loc (car (assoc *loc* *rooms*)))
         (next-room (look dir loc)))
    (cond (next-room (setf *loc* next-room) 
                     (where))
          (t '(ouch! robbie hit a wall)))))


;;; Start in the pantry.
(set-location 'pantry)

;;; Run Robbie through the house as per pg 190.
(move 'west)
(move 'west)
(move 'north)
(move 'north)
(move 'east)
(move 'south)
(move 'south)
(move 'east)





;;; Starting point from pg 316 onwards
;;; (load "tic-tac-toe.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Player := 1  internally
;;;          "O" on board
;;;
;;; Computer := 10  internally
;;;            "X"  on board
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *computer* 10)
(setf *player* 1)


(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))


;;; Original code from text.
;
;(defun convert-to-letter (v)
;  (cond ((equal v 1) "O")
;        ((equal v 10) "X")
;        (t " ")))
;
;(defun print-row-original (x y z)
;  (format t "~&   ~A | ~A | ~A"
;          (convert-to-letter x)
;          (convert-to-letter y)
;          (convert-to-letter z)))
;
;(defun print-row-modified-1 (x y z)
;  (labels ((to-letter (v)
;                      (cond ((equal v 1) "O")
;                            ((equal v 10) "X")
;                            (t " "))))
;    (format t "~&   ~A | ~A | ~A"
;            (to-letter x)
;            (to-letter y)
;            (to-letter z))))
;


;;; Refactored to move all helper methods into 
;;; here as local methods via labels.
(defun print-board (board)
  (labels ((convert-to-letter (v)
                      (cond ((equal v *player*) "O")
                            ((equal v *computer*) "X")
                            (t " ")))
           (print-row (x y z)
                      (format t "~&   ~A | ~A | ~A"
                        (convert-to-letter x)
                        (convert-to-letter y)
                        (convert-to-letter z))))
    (format t "~%")
    (print-row (nth 1 board) (nth 2 board) (nth 3 board))
    (format t "~&  ---+---+---")
    (print-row (nth 4 board) (nth 5 board) (nth 6 board))
    (format t "~&  ---+---+---")
    (print-row (nth 7 board) (nth 8 board) (nth 9 board))
    (format t "~%~%")))


;;; Test:
;;;(setf b (make-board))
;;;(print-board b)


;;; This modifies the board, but then also returns 
;;; the modified board?
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

;;; Revised version, modifies a copy of the 
;;; existing board and returns that. This keeps
;;; the board immutable, if that matters.
;(defun make-move (player pos board)
;  (let ((new-board (copy-list board)))
;    (setf (nth pos new-board) player)
;    new-board))


;;;(make-move *player* 3 b)
;;;(make-move *computer* 5 b)
;;;(print-board b)

;;;
;;; This is a map of all possible winning combinations
;;; given the design of the board.
;;;
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ; row wins
        (1 4 7) (2 5 8) (3 6 9) ; col wins
        (1 5 9) (3 5 7)))       ; diag wins

;;;
;;; Calculate the sum of a "row" on the board.
;;; Test (sum-triplet b '(3 5 7)) -> 11
;;;
(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

;;;
;;; Find the sums for each element of the *triplets* map.
;;;
(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))
;;; Test: (compute-sums b) -> (1 10 0 0 10 1 10 11)


(defun winner-p (board)
  "Is there a winner (either computer or player?"
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *player*) sums))))


(defun board-full-p (board)
  "Is the board full?"
  (not (member 0 board)))


(defun read-a-legal-move (board)
  "Get a valid move from the player and return it."
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; xxx-move are the actual game state machines that 
;;; drive game play. These recursively call each other,
;;; which for a game that could take an indeterminate
;;; amout of moves would be a problem, but here it's ok.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun player-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *player* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You Win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (computer-move new-board)))))


(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~$" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (player-move new-board)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computer strategies here.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "random move"))


(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos
      (pick-random-empty-position board))))


(defun find-empty-position (board squares)
  "Returns the first empty position in the board from 
  the query list squares, or nil if all square positions
  are occupied."
  (find-if #'(lambda (pos) (zerop (nth pos board)))
           squares))

(defun win-or-block (board target-sum)
  " Returns the position needed to either win or block."
  (let ((triplet (find-if #'(lambda (trip) 
                              (equal (sum-triplet board trip)
                                     target-sum))
                              *triplets*)))
    (when triplet (find-empty-position board triplet))))


(defun make-three-in-a-row (board)
  "Returns either (pos \"strategy\") or nil."
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos 
         (list pos "make three in a row"))))

(defun block-player-win (board)
  "Returns either (pos \"strategy\") or nil."
  (let ((pos (win-or-block board (* 2 *player*))))
    (and pos
         (list pos "block player"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard assignment(s), pgs 330-1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *corners* '(1 3 7 9))
(setf *sides* '(2 4 6 8))



;;; 1) Check diagonals for x-o-x
;;;    - Update, this should be o-x-o, since the computer
;;;      in this game is always x.
;;; 2) defend by picking a side square
;;; 3) else nil
(defun squeeze-play-diag-1-p (board)
  (if (and (equal (nth 1 board) *player*)
             (equal (nth 5 board) *computer*)
             (equal (nth 9 board) *player*))
    t nil))

(defun squeeze-play-diag-2-p (board)
  (if (and (equal (nth 3 board) *player*)
             (equal (nth 5 board) *computer*)
             (equal (nth 7 board) *player*))
    t nil))

(defun block-squeeze-play (board)
  (let ((pos (find-empty-position board *sides*)))
    (cond ((null pos) nil)
          ((squeeze-play-diag-1-p board)
           (list pos "Block squeeze play"))
          ((squeeze-play-diag-2-p board)
           (list pos "Block squeeze play"))
          (t nil))))

;;; Test setup:
;;; -----------
;;; (setf b (make-board))
;;; (make-move *player* 1 b)
;;; (make-move *computer* 5 b)
;;; (make-move *player* 9 b)
;;; (print-board b)
;;; (block-squeeze-play b)

(defun two-on-one-p (board)
  (cond ((and (equal (nth 1 board) *player*)
              (equal (nth 5 board) *player*)
              (equal (nth 9 board) *computer*))
         t)
        ((and (equal (nth 1 board) *computer*)
              (equal (nth 5 board) *player*)
              (equal (nth 9 board) *player*))
         t)
        ((and (equal (nth 3 board) *computer*)
              (equal (nth 5 board) *player*)
              (equal (nth 7 board) *player*))
         t)
        ((and (equal (nth 3 board) *player*)
              (equal (nth 5 board) *player*)
              (equal (nth 7 board) *computer*))
         t)
        (t nil)))

(defun block-two-on-one (board)
  (let ((pos (find-empty-position board *corners*)))
    (cond ((null pos) nil)
          ((two-on-one-p board)
           (list pos "Block two on one"))
          (t nil))))

;;; Test setup:
;;; -----------
;;; (setf b (make-board))
;;; (make-move *player* 1 b)
;;; (make-move *computer* 9 b)
;;; (make-move *player* 5 b)
;;; (print-board b)
;;; (block-two-on-one b)


(defun choose-best-move (board)
  "Order the strategies that the computer considers."
  (or (make-three-in-a-row board)
      (block-player-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (random-move-strategy board)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;             Main Entry Point
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun play-one-game ()
  "Starting point for this tic-tac-toe game."
  (if (y-or-n-p "Would you like to go first? ")
    (player-move (make-board))
    (computer-move (make-board))))




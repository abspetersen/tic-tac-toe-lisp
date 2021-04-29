;Abby Petersen
;Summary:
;tic tac toe game written in lisp using a 4x4 board: game is won by getting three of a symbol (X/O) in a row; either horizontally, vertically, or diagonally

;X: human player, O: AI player

;Initial board display is constructed using a list of “_” symbols: a function prints each nth value of the list (defined in its own function) with 4 symbols in each of the 4 rows

;Functions to determine the winner include: a winner function calls each function written to find the winner in a column, row and diagonal respectively. The function that determines if there has been a diagonal win calls a helper function that determines if there has been a diagonal win in any possible position

;Other functions implemented and used for basic functionality of game play include: a function that switched turns from human player to AI player, a function that replaces the nth item in the blank list (used for grid) with an element (the symbol (X/O) that represents a players move), function that fills in a human players move to the board (calls nth replace function), function that fills in an AI players move (calls nth-replace function)

;functions implemented for minimax search include: function that gets depth of a states accessor, function that gets weight accessor, function that keeps a record of the current state, parent, depth, weight and player and returns them in the correct order when given the state and its parent; function that gets the player accessor, a simple heuristic-eval function used to evaluate states, and a heuristic function that calls the heuristic-eval function, function that prompts human to make a move ;(calls fill-in-human play function), function that makes an AI player move, function that takes in a state and generates all of its descendants, function that finds the best move for the AI player, and the final function that execute the game

;Entire Sequence of game execution:
;Directions on how to enter move are displayed to human player
;Current player is displayed
;Winner function is called (winner function called functions for determining each type of win) in conditional statement: if a winner is found - “winner” is printed on the screen
;Inside tic tac toe function:
;Print board function is called that prints empty board
;Current player is displayed
;Winner function is called (winner function called functions for determining each type of win) in conditional statement:
;if a winner is found - “winner” is printed on the screen
;Else: recursive call with either the AI_play function of the humanMovePrompt function passed as a parameter (along with board variable) - depending on whose move it is (change-player function is called and passed as a parameter to the recursive call to determine this)
;The AI_play function: calls the minimax search function that determines the best move for the AI player by calling the generate-descendants and find_best_move functions (which are both recursive functions). The find-best-move function calls  the get-depth and get-weight functions. The generate descendants function calls the fill-in-computer, build record, and heuristic function (that calls the heuristic-eval function).
;The minimax function searches the closed list to find the best move if there are no more states to search. if there are still states to search, then get all the kids, add them to the open list and recurse, If max depth hasn't been reached - generate the offspring and continue search. the current state is added to the visited states and add the descendants to the open list. the current state is taken off the *open* list and recursive call is made
;Minimax function calls get-state, get-depth, generate-descendants, change player, and get-player functions and makes a recursive call


;tic tac toe game

(defvar maxPlays 3)
(defvar humanPlayer 'X)
(defvar AI_player 'O)
(defvar num_rows 4)
(defvar num_cols 4)
(defvar *debug* nil)



;function creates an empty board
(defun empty_board ()
  (list '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_ '_))

;function prints the board
(defun print_board (board)
  (format t "~%")
  (format t "~S ~S ~S ~S~%" (nth 0 board) (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~S ~S ~S ~S~%" (nth 4 board) (nth 5 board) (nth 6 board) (nth 7 board))
  (format t "~S ~S ~S ~S~%" (nth 8 board) (nth 9 board) (nth 10 board) (nth 11 board))
  (format t "~S ~S ~S ~S~%" (nth 12 board) (nth 13 board) (nth 14 board) (nth 15 board)))


;function determines if there is a winner in a row
(defun col_winner (col player board)
  (cond ((>= col num_cols) nil)
        ((and (equal (nth col board) (nth (+ col num_cols) board))
              (equal (nth (+ col num_cols) board) (nth (+ col (* 2 num_cols)) board))
              (equal (nth (+ col (* 2 num_cols)) board) player))
         player)
        (t (col_winner (+ col 1) player board))))


;determines if there is a winner in a column
(defun row_winner (row player board)
  (cond ((>= row num_rows) nil)
        ((and (equal (nth (* row num_cols) board) (nth (+ 1 (* row num_cols)) board))
              (equal (nth (+ 1 (* row num_cols)) board) (nth (+ 2 (* row num_cols)) board))
              (equal (nth (+ 2 (* row num_cols)) board) player))
         player)
        (t (row_winner (+ row 1) player board))))


;helper function for diagonal winner function - determines direction for a diagonal winner
(defun diagonal_winner_helper (player board direction)
  (cond ((and (equal direction 'r11-to-r33)
              (equal (nth 0 board) (nth 5 board))
              (equal (nth 5 board) (nth 10 board))
              (equal (nth 10 board) player))
         player)

         ((and (equal direction 'r22-to-r44)
              (equal (nth 5 board) (nth 10 board))
              (equal (nth 10 board) (nth 15 board))
              (equal (nth 15 board) player))
                player)
          (t nil))

  (cond ((and (equal direction'r14-to-r32)
          (equal (nth 4 board) (nth 7 board))
          (equal (nth 7 board) (nth 10 board))
          (equal (nth 10 board) player))
         player)

         ((and (equal direction 'r23-to-r41)
               (equal (nth 7 board) (nth 10 board))
               (equal (nth 10 board) (nth 13 board))
               (equal (nth 13 board) player))
          player)
        (t nil)))

;function that calls diagonal winner helper function to determine if there is a diagonal winner
(defun diagonal_winner (player board)
  (or (diagonal_winner_helper player board 'r11-to-r33)
      (diagonal_winner_helper player board 'r22-to-r44)
      (diagonal_winner_helper player board 'r14-to-r32)
      (diagonal_winner_helper player board 'r23-to-r41)))

;function that calls other functions for winners in different directions - to determine if there is a winner
(defun winner (board)
  (or (col_winner 0 humanPlayer board)
      (row_winner 0 humanPlayer board)
      (diagonal_winner humanPlayer board)
      (col_winner 0 AI_player board)
      (row_winner 0 AI_player board)
      (diagonal_winner AI_player board)))


;function changes turns between the human player and the AI player
(defun change-player (player)
  (if (equal player humanPlayer) AI_player humanPlayer))

;function replaces the nth item in a list with elem
(defun replace-nth (lst n elem)
  (cond
    ((null lst) nil)
    ((= n 0) (cons elem (cdr lst)))
    (t (cons (car lst) (replace-nth (cdr lst) (- n 1) elem)))))

;function put item in a square - for human player
(defun fill-in-human (player posn board)
  (if (equal '_ (nth (- posn 1) board))     ;checks to see if the space is open
    (replace-nth board (- posn 1) player)   ;if it's empty, put the player's marker there
    nil))                                   ;otherwise, return nil

;put item in a square - for computer player
(defun fill-in-computer (player posn board)
(if (equal '_ (nth posn board))
  (replace-nth board posn player)
  nil))

;functions for the minimax search are below this point

(defvar *squares* '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
(defvar *open*)
(defvar closed)


;function returns a list (of state, parent, depth, weight, player)- given the state and it's parent
(defun build-record (state parent depth weight player)
  (list state parent depth weight player))

;function gets depth accessor
(defun get-depth (state-tuple)
  (nth 2 state-tuple))

;function gets a state's accessor - the first element in the list is the state
(defun get-state (state-tuple)
  (nth 0 state-tuple))


;function gets weight accessor -  value for choosing best option to search next
(defun get-weight (state-tuple)
  (nth 3 state-tuple))

;function gets player accessor - which player is associated with that state
(defun get-player (state-tuple)
  (nth 4 state-tuple))


;heuristic function used to evaluate states - counts the number of players that are not in their goal positions
(defun heuristic-eval (state)
  (random 10))

(defun heuristic (state)
 (heuristic-eval state))


;function takes in a state and generates all of its descendants using the function calls in the "moves" list
;things keep getting picked off the list until we get to the last move
;checks if the state is already in the list of children that have been generated
;checks if the state is already in the list of open states we need to check
;checks if the state is in the list of states we've already checked - if it's a new state, add it to our list
;function is called in minimax function

(defun generate_descendants (board squares depth player)
  (cond ((null squares) nil)
      (t   (let ((child (fill-in-computer player (car squares) board))
        (restOfList  (generate_descendants board (cdr squares) depth player)))
              (cond ((null child) restOfList)
                ((member child restOfList :test #'equal) restOfList)
                  ((member child *open* :test #'equal)     restOfList)
                    ((member child closed :test #'equal)   restOfList)
                      (t (cons (build-record child board depth (heuristic child) player) restOfList)))))))


;function that finds the best move for the AI player - it goes through the closed list to pick the best move -
;since the current board has depth 0, it finds the node of
;depth 1 with the highest heuristic score
;called in minimax function
(defun find-best-move (max-posn max-weight current-posn)
  (cond (*debug* (format t "max-posn ~S~%" max-posn)
                 (format t "max-weight ~S~%" max-weight)
                 (format t "current-posn ~S~%" current-posn)))
  (cond ((>= current-posn (length closed)) max-posn)
        (t
         (let ((current-state (nth current-posn closed)))
           (if *debug* (format t "depth ~S weight ~S~%" (get-depth current-state) (get-weight current-state)))
           (if (and (= 1 (get-depth current-state))
                (< max-weight (get-weight current-state)))
             (find-best-move current-posn (get-weight current-state) (+ current-posn 1))
             (find-best-move max-posn max-weight (+ current-posn 1)))))))


;function for minimax algorithm - if there are no more states to search - closed list is searched to find the best move
;if there are still states to search, then get all the kids, add them to the open list and recurse
;If max depth hasn't been reached - generate the offspring and continue search
;the current state is added to the visited states and add the descendants to the open list
;the current state is taken off the *open* list and recursive call is made
(defun minimax ()
  (cond (nil (print "open") (print *open*)
             (print "closed") (print closed)))
  (cond
        ((null *open*) (get-state (nth (find-best-move -1 -1 0) closed)))
        (t
         (let ((state-record (car *open*)))
           (if nil (format t "state-record ~S~%" state-record))
           (cond
                 ((> maxPlays (get-depth state-record))
                  (setq closed (cons state-record closed))
                  (setq *open* (append (generate_descendants
                                        (get-state state-record)
                                        *squares*
                                        (+ 1 (get-depth state-record))
                                        (change-player (get-player state-record)))
                                       (cdr *open*))))
                 (t
                  (setq closed (cons state-record closed))
                  (setq *open* (cdr *open*)))))
         (minimax))))


;function that makes an AI play - uses minimax function
;the current state of the board is searched (build record and heuristic functions are called)
;closed list is empty and minimax function is called
(defun AI_play (board)
  (setq *open* (list (build-record board nil 0 (heuristic board) humanPlayer)))
  (setq closed nil)
  (minimax)
)

;function that prompts human player to make their move - human is asked for a move, read gets the move, and the fill function puts that move on the board
(defun humanMovePrompt (board)
  (print "choose a square")
  (fill-in-human humanPlayer (read) board)
)


;function executes game - it prints the board and checks to see if someone has won. If nobody has won, next person plays their move
;calls winner, change player, humanMovePrompt, AI_player functions
;calls change player, human move prompt, and AI player functions in recursive call
(defun tic_tac_toe (player board)
  (print_board board)
  (print "Player:")
  (print player)
  (cond ((winner board) (print "winner") (winner board))
        ((equal player humanPlayer) (tic_tac_toe (change-player player) (humanMovePrompt board)))
        (t  (tic_tac_toe (change-player player) (AI_play board)))))


;prompt on how to enter your move
(print "Enter the number corresponding to the square you want to move to - moving from left to right, top to bottom - starting at 1 and ending with 15")

;function call that executes game play
(tic_tac_toe humanPlayer (empty_board))

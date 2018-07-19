#lang racket
(require racket/stxparam
         web-server/servlet
         web-server/servlet-env
         racket/tcp
         srfi/25)

;; Define some constants for the Battleship game
(define field-size 10)
(define start-game #f)

;; struct for all ship
;; bow-row -> int, bow-column -> int, length -> int, horizontal -> boolean,
;; hit -> a list of boolean to indicate the hits, I decide to change it to a bool
; display -> string, "B" for battleship, "C" for cruiser, "D" for destroyer, "S" for submarine
(struct ship (bow-row bow-column length horizontal hits display))

; get the length of the ship
(define (get-length-of-ship a-ship)
  (if (ship? a-ship)
      (ship-length a-ship)
      (raise "[get-length-of-ship] a-ship is not a ship struct.")))

; get the bow-row of the ship
(define (get-bow-row-of-ship a-ship)
  (if (ship? a-ship)
      (ship-bow-row a-ship)
      (raise "[get-bow-row-of-ship] a-ship is not a ship struct.")))

; get the bow-column of the ship
(define (get-bow-column-of-ship a-ship)
  (if (ship? a-ship)
      (ship-bow-column a-ship)
      (raise "[get-bow-column-of-ship] a-ship is not a ship struct.")))

; get the horizontality of the ship
(define (is-horizontal a-ship)
  (if (ship? a-ship)
      (ship-horizontal a-ship)
      (raise "[is-horizontal] a-ship is not a ship struct.")))

; get the length of the ship
(define (get-hit-of-ship a-ship)
  (if (ship? a-ship)
      (ship-hits a-ship)
      (raise "[get-hit-of-ship] a-ship is not a ship struct.")))

; set the bow row of the ship
(define (set-bow-row-of-ship a-ship row)
  (if (ship? a-ship)
      (ship row (ship-bow-column a-ship) (ship-length a-ship) (ship-horizontal a-ship) (ship-hits a-ship))
      (raise "[set-bow-row-of-ship] a-ship is not a ship struct.")))

; set the bow column of the ship
(define (set-bow-column-of-ship a-ship column)
  (if (ship? a-ship)
      (ship (ship-bow-row a-ship) column (ship-length a-ship) (ship-horizontal a-ship) (ship-hits a-ship))
      (raise "[set-bow-column-of-ship] a-ship is not a ship struct.")))

; set the horizontal of the ship
(define (set-horizontal-of-ship a-ship horizontal)
  (if (ship? a-ship)
      (ship (ship-bow-row a-ship) (ship-bow-column a-ship) (ship-length a-ship) horizontal (ship-hits a-ship))
      (raise "[set-horizontal-of-ship] a-ship is not a ship struct.")))

; get the type of a ship
(define (get-ship-type a-ship)
  (if (ship? a-ship)
      (cond
        [(= (ship-length a-ship) 4)
         "Battleship"]
        [(= (ship-length a-ship) 3)
         "Cruiser"]
        [(= (ship-length a-ship) 2)
         "Destroyer"]
        [(= (ship-length a-ship) 1)
         "Submarine"]
        [else
         (displayln "not a ship supported by my Battleship game.")])
      (raise "[get-ship-type] a-ship is not a ship struct.")))

; a helper function that checks the adjancency occupation around
; the location given by the row and column.
; This one is for vertical
; It seems to work now.
(define (is-occupation-in-ocean-helper-vertical row column a-ship an-ocean)
  (cond [(equal? a-ship "Battleship")
         (if (or (>= (+ row 1) 10) (>= (+ row 2) 10) (>= (+ row 3) 10))
             #f
             (nor (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 2) column an-ocean)
                  (is-occupied (+ row 3) column an-ocean)
                  (is-occupied (+ row 4) column an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied row (- column 1) an-ocean)
                  (is-occupied row (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 2) (- column 1) an-ocean)
                  (is-occupied (+ row 2) (+ column 1) an-ocean)
                  (is-occupied (+ row 3) (- column 1) an-ocean)
                  (is-occupied (+ row 3) (+ column 1) an-ocean)
                  (is-occupied (+ row 4) (- column 1) an-ocean)
                  (is-occupied (+ row 4) (+ column 1) an-ocean)))]
        [(equal? a-ship "Cruiser")
         (if (or (>= (+ row 1) 10) (>= (+ row 2) 10))
             #f
             (nor (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 2) column an-ocean)
                  (is-occupied (+ row 3) column an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied row (- column 1) an-ocean)
                  (is-occupied row (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 2) (- column 1) an-ocean)
                  (is-occupied (+ row 2) (+ column 1) an-ocean)
                  (is-occupied (+ row 3) (- column 1) an-ocean)
                  (is-occupied (+ row 3) (+ column 1) an-ocean)))]
        [(equal? a-ship "Destroyer")
         (if (>= (+ row 1) 10)
             #f
             (nor (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 2) column an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied row (- column 1) an-ocean)
                  (is-occupied row (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 2) (- column 1) an-ocean)
                  (is-occupied (+ row 2) (+ column 1) an-ocean)))]
        [(equal? a-ship "Submarine")
         (nor (is-occupied (+ row 1) column an-ocean)
              (is-occupied (- row 1) column an-ocean)
              (is-occupied (- row 1) (+ column 1) an-ocean)
              (is-occupied (- row 1) (- column 1) an-ocean)
              (is-occupied row (- column 1) an-ocean)
              (is-occupied row (+ column 1) an-ocean)
              (is-occupied (+ row 1) (- column 1) an-ocean)
              (is-occupied (+ row 1) (+ column 1) an-ocean))]
        [else (raise "a-ship is not a valid ship")]))

; a helper function that checks the adjancency occupation around
; the location given by the row and column.
; This one is for horizontal
; It seems to work now.
(define (is-occupation-in-ocean-helper-horizontal row column a-ship an-ocean)
  (cond [(equal? a-ship "Battleship")
         (if (or (>= (+ column 1) 10) (>= (+ column 2) 10) (>= (+ column 3) 10))
             #f
             (nor (is-occupied row (+ column 1) an-ocean)
                  (is-occupied row (+ column 2) an-ocean)
                  (is-occupied row (+ column 3) an-ocean)
                  (is-occupied row (+ column 4) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 2) an-ocean)
                  (is-occupied (- row 1) (+ column 2) an-ocean)
                  (is-occupied (+ row 1) (+ column 3) an-ocean)
                  (is-occupied (- row 1) (+ column 3) an-ocean)
                  (is-occupied (+ row 1) (+ column 4) an-ocean)
                  (is-occupied (- row 1) (+ column 4) an-ocean)))]
        [(equal? a-ship "Cruiser")
         (if (or (>= (+ column 1) 10) (>= (+ column 2) 10))
             #f
             (nor (is-occupied row (+ column 1) an-ocean)
                  (is-occupied row (+ column 2) an-ocean)
                  (is-occupied row (+ column 3) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 2) an-ocean)
                  (is-occupied (- row 1) (+ column 2) an-ocean)
                  (is-occupied (+ row 1) (+ column 3) an-ocean)
                  (is-occupied (- row 1) (+ column 3) an-ocean)))]
        [(equal? a-ship "Destroyer")
         (if (>= (+ column 1) 10)
             #f
             (nor (is-occupied row (+ column 1) an-ocean)
                  (is-occupied row (+ column 2) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (+ row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) (- column 1) an-ocean)
                  (is-occupied (- row 1) column an-ocean)
                  (is-occupied (+ row 1) column an-ocean)
                  (is-occupied (+ row 1) (+ column 1) an-ocean)
                  (is-occupied (- row 1) (+ column 1) an-ocean)
                  (is-occupied (+ row 1) (+ column 2) an-ocean)
                  (is-occupied (- row 1) (+ column 2) an-ocean)))]
        [(equal? a-ship "Submarine")
         (nor (is-occupied (+ row 1) column an-ocean)
              (is-occupied (- row 1) column an-ocean)
              (is-occupied (- row 1) (+ column 1) an-ocean)
              (is-occupied (- row 1) (- column 1) an-ocean)
              (is-occupied row (- column 1) an-ocean)
              (is-occupied row (+ column 1) an-ocean)
              (is-occupied (+ row 1) (- column 1) an-ocean)
              (is-occupied (+ row 1) (+ column 1) an-ocean))]
        [else (raise "a-ship is not a valid ship")]))        

; Check if it is ok to place the ship at this location
; Return true if it is ok to place the ship of this length at the location with
; given orientation. Otherwise false.
; The ship cannot overlap another ship, or touch another ship.
; The minimum distance between two ships is 1. And it cannot go beyond the 10x10 ocean board.
; It seems to work now.
(define (ok-to-place-ship-at row column horizontal an-ocean a-ship)
  (if (ocean-rep? an-ocean)
      (if (and (< row 10) (>= row 0) (< column 10) (>= column 0))
          ;check if the location given by row and column is occupied or not
          (if (not (is-occupied row column an-ocean))
              ; if it is not occupied, check its orientation
              (if horizontal
                  ; if it is horizontal
                  (is-occupation-in-ocean-helper-horizontal row column a-ship an-ocean)
                  ; else, it is vertical
                  (is-occupation-in-ocean-helper-vertical row column a-ship an-ocean))
              ; else, return false
              #f)
          #f)
      (raise "[ok-to-place-ship-at] an-ocean is not an ocean struct")))

; place the ship at a particular location
; Need to be careful about the length of the ship
; Return true if placing the ship is successful. Otherwise false.
; It seems to work now.
(define (place-ship-at row column horizontal an-ocean a-ship)
  (if (and (ocean-rep? an-ocean))
      (cond [(ok-to-place-ship-at row column horizontal an-ocean a-ship)
             (if horizontal
                 ; place the ship according to the type of the ship
                 (cond [(equal? a-ship "Battleship")
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                    (ship row (+ column 1) 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 2)
                                    (ship row (+ column 2) 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 3)
                                    (ship row (+ column 3) 4 horizontal #f "B"))
                        #t]
                       [(equal? a-ship "Cruiser")
                        (array-set! (ocean-rep-ships-array an-ocean) row column
                                    (ship row column 3 horizontal #f "C"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                    (ship row (+ column 1) 3 horizontal #f "C"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 2)
                                    (ship row (+ column 2) 3 horizontal #f "C"))
                        #t]
                       [(equal? a-ship "Destroyer")
                        (array-set! (ocean-rep-ships-array an-ocean) row column
                                    (ship row column 2 horizontal #f "D"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                    (ship row (+ column 1) 2 horizontal #f "D"))
                        #t]
                       [(equal? a-ship "Submarine")
                        (array-set! (ocean-rep-ships-array an-ocean) row column
                                    (ship row column 1 horizontal #f "S"))
                        #t])
                 (cond [(equal? a-ship "Battleship")
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 1) column
                                    (ship (+ row 1) column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 2) column
                                    (ship (+ row 2) column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 3) column
                                    (ship (+ row 3) column 4 horizontal #f "B"))
                        #t]
                       [(equal? a-ship "Cruiser")
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 3 horizontal #f "C"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 1) column
                                    (ship (+ row 1) column 3 horizontal #f "C"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 2) column
                                    (ship (+ row 2) column 3 horizontal #f "C"))
                        #t]
                       [(equal? a-ship "Destroyer")
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 2 horizontal #f "D"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 1) column
                                    (ship (+ row 1) column 2 horizontal #f "D"))
                        #t]
                       [(equal? a-ship "Submarine")
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 1 horizontal #f "S"))
                        #t]))]
            [else (displayln "Illegal placement of the ship.")
                  #f])
      (raise "[place-ship-at] an-ocean is not an ocean struct or a-ship is not a ship struct.")))

; check if the ship has been sunk or not by looking at the ship's hits list.
; Return true if it is sunk. Otherwise, false.
(define (is-sunk a-ship)
  ;(andmap (λ (x) (and x)) (ship-hits a-ship))
  (get-hit-of-ship a-ship))

; shoot at a particular location in the ocean
; Return true and mark the corresponding part of the hits list of the ship
; as true if a part of the ship occupies the given row and column and it isn't sunk.
; Otherwise false.
; not tested yet. Potential problematic!!!!
(define (shoot-at row column an-ocean)
  (if (ocean-rep? an-ocean)
      (if (and (< row 10) (>= row 0) (< column 10) (>= column 0))
          (let* ([the-target-ship (array-ref (ocean-rep-ships-array an-ocean) row column)]
                 [the-type-of-target (get-ship-type the-target-ship)])
            (if (and (> (ship-length the-target-ship) 0) (not (is-sunk the-target-ship)))
                (cond [(equal? the-type-of-target "Battleship")
                       (array-set! (ocean-rep-ships-array an-ocean) row column 
                                   (ship row column 4 (is-horizontal the-target-ship) #t "H"))
                       #t]
                      [(equal? the-type-of-target "Cruiser")
                       (array-set! (ocean-rep-ships-array an-ocean) row column 
                                   (ship row column 3 (is-horizontal the-target-ship) #t "H"))
                       #t]
                      [(equal? the-type-of-target "Destroyer")
                       (array-set! (ocean-rep-ships-array an-ocean) row column 
                                   (ship row column 2 (is-horizontal the-target-ship) #t "H"))
                       #t]
                      [(equal? the-type-of-target "Submarine")
                       (array-set! (ocean-rep-ships-array an-ocean) row column 
                                   (ship row column 1 (is-horizontal the-target-ship) #t "H"))
                       #t])
                #f))
          #f)
      (raise "an-ocean is not an ocean struct.")))

; Ocean struct
(struct ocean-rep (ships-array shots-fired hit-count ships-sunk))

; check if a location in the ocean is occupied by a ship
; Return true if it is occupied by a ship. Otherwise, false.
(define (is-occupied row column an-ocean)
  (if (ocean-rep? an-ocean)
      (if (and (< row 10) (>= row 0) (< column 10) (>= column 0))
          (not (= (ship-length (array-ref (ocean-rep-ships-array an-ocean) row column)) 0))
          #f)
      (raise "an-ocean is not an ocean struct")))

; shoot at a location given by row and column
; Return true if the given location has a floating ship. Otherwise false.
; At the same time we need to update the number of shots that have been fired,
; and the number of hits.
; not tested yet. Potential problematic!!!!
(define (count-shoot-at row column an-ocean)
  (shoot-at row column an-ocean))

; get the number of shots fired in the ocean
(define (get-shots-fired an-ocean)
  (if (ocean-rep? an-ocean)
      (ocean-rep-shots-fired an-ocean)
      (raise "an-ocean is not an ocean struct")))

; get the hit count of the ocean
(define (get-hit-count an-ocean)
  (if (ocean-rep? an-ocean)
      (ocean-rep-hit-count an-ocean)
      (raise "an-ocean is not an ocean struct")))

; get the number of ships sunk in the ocean
(define (get-num-of-ships-sunk an-ocean)
  (if (ocean-rep? an-ocean)
      (ocean-rep-ships-sunk an-ocean)
      (raise "an-ocean is not an ocean struct")))

; Check if the game is over
; Return true if all ships have been sunk. Otherwise false
(define (is-game-over an-ocean)
  (if (ocean-rep? an-ocean)
      (>= (get-num-of-ships-sunk an-ocean) 20)
      (raise "an-ocean is not an ocean struct")))

;; Let's build the Battleship game now

; Initialize an ocean that has no ship at all
; maybe create a box to wrap around it so that we have a reference???
(define ocean (ocean-rep (make-array (shape 0 10 0 10) (ship 0 0 0 #t #f "O")) 0 0 0))

; The visualization of the ocean
(define ocean-board (make-array (shape 0 10 0 10) "O"))

; print out the board for visualization
(define (pretty-print board)
  (displayln "  0 1 2 3 4 5 6 7 8 9")
  (for ([i (in-range (array-length (ocean-rep-ships-array board) 0))])
    (display (string-append (number->string i) " "))
    (for ([j (in-range (array-length (ocean-rep-ships-array board) 1))])
      (display (string-append (ship-display (array-ref (ocean-rep-ships-array board) i j)) " ")))
    (newline)))

; Initially the number of ships available player 1 is (B-1 C-2 D-3 S-4). The same for player 2.
(define num-of-ships-placed-p1 (list 1 2 3 4))
(define num-of-ships-placed-p2 (list 1 2 3 4))

; get the sum of the ships placed by a player
(define (sum-of-ships-placed ship-num-list)
  (foldl + 0 ship-num-list))

; a helper function for run-game to place the ship
; Return true if placing ship is successful. Otherwise, display the message.
(define (run-game-place-ship-helper player-input-tokens ocean)
  (cond [(equal? (last player-input-tokens) "horizontal")
         (place-ship-at (string->number (third player-input-tokens)) (string->number (fourth player-input-tokens)) #t ocean (second player-input-tokens))]
        [(equal? (last player-input-tokens) "vertical")
         (place-ship-at (string->number (third player-input-tokens)) (string->number (fourth player-input-tokens)) #f ocean (second player-input-tokens))]
        [else (displayln "Illegal orientation")
              #f]))

; a function that runs the game. It doesn't work with the web server yet.
(define (game-on)
  (displayln "!!!Welcome to the Battleship game!!!")
  (displayln "This game is for two players who place different battleships on the ocean")
  (displayln "and try to sink all the ships of another player in order to win the game.")
  (newline)
  (displayln "In this game each player has 10 ships, 1 Battleship of length 4, 2 Cruiser")
  (displayln "of length 3, 3 Destroyers of length 2, and 4 Submarines of length 1.")
  (displayln "Please stand by for another player to join the game.") ; lets run a game first before worrying about the 2nd player
  (newline)
  (displayln "We have a second player joining the game! Let's start the game!")
  (newline)
  (displayln "Initializing your ocean board.")
  (pretty-print ocean)
  (newline)
  (displayln "First we need to place 10 ships on the board.")

  (displayln "To place the ships, you need to input \"place\", the type of the ship, row, column, and its orientation.")
  (displayln "Example: place Battleship 0 0 horizontal. But you cannot place more than 1 Battleship.")
  (define (run-game)
    (let* ([player-input (read-line)]
           [player-input-tokens (string-split player-input)])
      (cond [(or (equal? (car player-input-tokens) "exit") (equal? (car player-input-tokens) "quit"))
             (raise "Player exits the battleship game.")]
            [(equal? (car player-input-tokens) "place")
             ; first, need to check the number of ships the player can place
             (cond [(> (sum-of-ships-placed num-of-ships-placed-p1) 0)
                    ; ships placements for Battleship, Destroyer, Cruiser, and Submarine
                    (cond [(equal? (second player-input-tokens) "Battleship")
                           (cond [(<= (first num-of-ships-placed-p1) 0)
                                  (displayln "No more Battleship for deployment.")]
                                 [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                           (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 0 (- (first num-of-ships-placed-p1) 1)))
                                           (void))])]
                          [(equal? (second player-input-tokens) "Cruiser")
                           (cond [(<= (second num-of-ships-placed-p1) 0)
                                  (displayln "No more Cruiser for deployment.")]
                                 [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                           (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 0 (- (second num-of-ships-placed-p1) 1)))
                                           (void))])]
                          [(equal? (second player-input-tokens) "Destroyer")
                           (cond [(<= (third num-of-ships-placed-p1) 0)
                                  (displayln "No more Destroyer for deployment.")]
                                 [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                           (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 0 (- (third num-of-ships-placed-p1) 1)))
                                           (void))])]
                          [(equal? (second player-input-tokens) "Submarine")
                           (cond [(<= (fourth num-of-ships-placed-p1) 0)
                                  (displayln "No more Submarine for deployment.")]
                                 [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                           (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 0 (- (fourth num-of-ships-placed-p1) 1)))
                                           (void))])])]
                   [else (displayln "All ships have been successfully deployed.")])]
            [else (run-game)])
      (newline)
      (displayln "Now your ocean board looks like:")
      (pretty-print ocean)
      (run-game)
      ))
  (run-game))

(game-on)
;; Let's start with a basic chat server from Rosetta code: https://rosettacode.org/wiki/Chat_server#Racket
;; This may serve as the foundation of the battleship game
;; I will modify it to be completely different from the original code
#|(define outs (list (current-output-port)))
(define ((tell-all who o) line)
  (for ([c outs] #:unless (eq? o c)) (displayln (~a who ": " line) c)))
 
(define ((player i o))
  (define player-screen (begin (displayln "Welcome to the Battleship!")))
  (define nick (begin (display "Nick: " o) (read-line i)))
  (define tell (tell-all nick o))
  (let loop ([line "(joined)"])
    (if (eof-object? line)
      (begin (tell "(left)") (set! outs (remq o outs)) (close-output-port o))
      (begin (tell line) player-screen (loop (read-line i))))))
 
(define (battleship listener)
  (define-values [i o] (tcp-accept listener))
  (for ([p (list i o)]) (file-stream-buffer-mode p 'none))
  (thread (player i o)) (set! outs (cons o outs)) (battleship listener))
 
(void (thread (λ () (battleship (tcp-listen 8080)))))
((player (current-input-port) (current-output-port)))
|#
#|
(define (handle in out)
  ; Discard the request header (up to blank line):
  (regexp-match #rx"(\r\n|^)\r\n" in)
  ; Send reply:
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
  

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out)
      (tcp-accept listener))
    (thread
     (λ ()
       (handle in out)
       (close-input-port in)
       (close-output-port out))))
  ; Watcher thread, close everything when a thread exits:
  (thread (λ ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (server port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (λ ()
    (custodian-shutdown-all main-cust)))
|#

#|(let*
    ([empty (λ (iscons isnull)
                  (isnull))]
     [makecons (λ (a b)
                 (λ (iscons isnull)
                   (iscons a b)))]
     [car (λ (lst) (lst (λ (a b) a) (λ () #f)))]
     [cdr (λ (lst) (lst (λ (a b) b) (λ () #f)))]
     [null? (λ (lst) (lst (λ (a b) #f) (λ () #t)))]
     [cons? (λ (lst) (lst (λ (a b) #t) (λ () #f)))]
     [makeboard (λ () empty)]
     [add-piece (λ (board x y)
                  (makecons (makecons x y) board))]
     [Y3 ((λ (u) (u u))
          (λ (y) (λ (f) (f (λ (a0 a1 a2)
                             (((y y) f) a0 a1 a2))))))]
     [mark-hit (Y3 (λ (mark-hit)
                     (λ (board x y)
                       (if (null? board)
                           (makecons board #f)
                           (let* ([fst (car board)]
                                  [rst (cdr board)])
                             (if (and (= (car fst) x)
                                      (= (cdr fst) y))
                                 (makecons rst #t)
                                 (let ([rst+b (mark-hit rst x y)])
                                   (makecons (makecons fst
                                                       (car rst+b))
                                             (cdr rst+b)))))))))]
     [isdead? (λ (board) (null? board))])
  (let-label
   p1 
   x1
   (= 1 x1)
   (let-label
    p2
    x2
    (= 2 x2)
    ;; Simulate a game
    (let* ([b1 (makeboard)]
           [b1 (add-piece b1 2 3)]
           [b1 (add-piece b1 1 3)]
           [b1 (add-piece b1 1 1)]
           [b1 (add-piece b1 5 5)]
           [b1 (add-piece b1 4 4)]
           [b2 (makeboard)]
           [b2 (add-piece b2 1 1)]
           [b2 (add-piece b2 3 4)]
           [b2 (add-piece b2 4 5)]
           [b2 (add-piece b2 1 5)]
           [b2 (add-piece b2 4 1)]
           [s1 (facet p1 b1 (makeboard))]
           [s2 (facet p2 b2 (makeboard))]
           [res (mark-hit s1 1 1)]
           [b (obs p1 1 (cdr res))])
      b))))
|#
;#lang racket
#lang reader "racets.rkt"
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
        (if (= (ship-length a-ship) 4)
            "Battleship"
            (if (= (ship-length a-ship) 3)
                "Cruiser"
                (if (= (ship-length a-ship) 2)
                    "Destroyer"
                    (if (= (ship-length a-ship) 1)
                        "Submarine"
                        (displayln "Not a ship supported by my Battleship game, or it is empty sea.")))))
        (raise "[get-ship-type] a-ship is not a ship struct.")))

; a helper function that checks the adjancency occupation around
; the location given by the row and column.
; This one is for vertical
; It seems to work now.
(define (is-occupation-in-ocean-helper-vertical row column a-ship an-ocean)
    (if (equal? a-ship "Battleship")
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
                 (is-occupied (+ row 4) (+ column 1) an-ocean)))
        (if (equal? a-ship "Cruiser")
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
                     (is-occupied (+ row 3) (+ column 1) an-ocean)))
            (if (equal? a-ship "Destroyer")
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
                         (is-occupied (+ row 2) (+ column 1) an-ocean)))
                (if (equal? a-ship "Submarine")
                    (nor (is-occupied (+ row 1) column an-ocean)
                         (is-occupied (- row 1) column an-ocean)
                         (is-occupied (- row 1) (+ column 1) an-ocean)
                         (is-occupied (- row 1) (- column 1) an-ocean)
                         (is-occupied row (- column 1) an-ocean)
                         (is-occupied row (+ column 1) an-ocean)
                         (is-occupied (+ row 1) (- column 1) an-ocean)
                         (is-occupied (+ row 1) (+ column 1) an-ocean))
                    (raise "a-ship is not a valid ship."))))))

; a helper function that checks the adjancency occupation around
; the location given by the row and column.
; This one is for horizontal
; It seems to work now.
(define (is-occupation-in-ocean-helper-horizontal row column a-ship an-ocean)
    (if (equal? a-ship "Battleship")
        (if (or (>= (+ column 1) 10) (>= (+ column 2) 10) (>= (+ column 3) 10))
            #f
            (nor (is-occupied row (+ column 1) an-ocean)
                 (is-occupied row (+ column 2) an-ocean)
                 (is-occupied row (+ column 3) an-ocean)
                 (is-occupied row (+ column 4) an-ocean)
                 (is-occupied row (- column 1) an-ocean)
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
                 (is-occupied (- row 1) (+ column 4) an-ocean)))
        (if (equal? a-ship "Cruiser")
            (if (or (>= (+ column 1) 10) (>= (+ column 2) 10))
                #f
                (nor (is-occupied row (+ column 1) an-ocean)
                     (is-occupied row (+ column 2) an-ocean)
                     (is-occupied row (+ column 3) an-ocean)
                     (is-occupied row (- column 1) an-ocean)
                     (is-occupied (+ row 1) (- column 1) an-ocean)
                     (is-occupied (- row 1) (- column 1) an-ocean)
                     (is-occupied (- row 1) column an-ocean)
                     (is-occupied (+ row 1) column an-ocean)
                     (is-occupied (+ row 1) (+ column 1) an-ocean)
                     (is-occupied (- row 1) (+ column 1) an-ocean)
                     (is-occupied (+ row 1) (+ column 2) an-ocean)
                     (is-occupied (- row 1) (+ column 2) an-ocean)
                     (is-occupied (+ row 1) (+ column 3) an-ocean)
                     (is-occupied (- row 1) (+ column 3) an-ocean)))
            (if (equal? a-ship "Destroyer")
                (if (>= (+ column 1) 10)
                    #f
                    (nor (is-occupied row (+ column 1) an-ocean)
                         (is-occupied row (+ column 2) an-ocean)
                         (is-occupied row (- column 1) an-ocean)
                         (is-occupied (+ row 1) (- column 1) an-ocean)
                         (is-occupied (- row 1) (- column 1) an-ocean)
                         (is-occupied (- row 1) column an-ocean)
                         (is-occupied (+ row 1) column an-ocean)
                         (is-occupied (+ row 1) (+ column 1) an-ocean)
                         (is-occupied (- row 1) (+ column 1) an-ocean)
                         (is-occupied (+ row 1) (+ column 2) an-ocean)
                         (is-occupied (- row 1) (+ column 2) an-ocean)))
                (if (equal? a-ship "Submarine")
                    (nor (is-occupied row (+ column 1) an-ocean)
                         (is-occupied row (- column 1) an-ocean)
                         (is-occupied (+ row 1) (- column 1) an-ocean)
                         (is-occupied (- row 1) (- column 1) an-ocean)
                         (is-occupied (- row 1) column an-ocean)
                         (is-occupied (+ row 1) column an-ocean)
                         (is-occupied (+ row 1) (+ column 1) an-ocean)
                         (is-occupied (- row 1) (+ column 1) an-ocean))
                    (raise "a-ship is not a valid ship."))))))

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
; I change the ship display to be "O" in order to hide the ship.
(define (place-ship-at row column horizontal an-ocean a-ship)
    (if (ocean-rep? an-ocean)
        (if (ok-to-place-ship-at row column horizontal an-ocean a-ship)
            (begin
              (if horizontal
                  (if (equal? a-ship "Battleship")
                      (begin
                        (array-set! (ocean-rep-ships-array an-ocean) row column 
                                    (ship row column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                    (ship row (+ column 1) 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 2)
                                    (ship row (+ column 2) 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) row (+ column 3)
                                    (ship row (+ column 3) 4 horizontal #f "B")) #t)
                      (if (equal? a-ship "Cruiser")
                          (begin
                            (array-set! (ocean-rep-ships-array an-ocean) row column
                                        (ship row column 3 horizontal #f "C"))
                            (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                        (ship row (+ column 1) 3 horizontal #f "C"))
                            (array-set! (ocean-rep-ships-array an-ocean) row (+ column 2)
                                        (ship row (+ column 2) 3 horizontal #f "C")) #t)
                          (if (equal? a-ship "Destroyer")
                              (begin
                                (array-set! (ocean-rep-ships-array an-ocean) row column
                                            (ship row column 2 horizontal #f "D"))
                                (array-set! (ocean-rep-ships-array an-ocean) row (+ column 1)
                                            (ship row (+ column 1) 2 horizontal #f "D")) #t)
                              (if (equal? a-ship "Submarine")
                                  (begin
                                    (array-set! (ocean-rep-ships-array an-ocean) row column
                                                (ship row column 1 horizontal #f "S")) #t)
                                  (void)))))
                  (if (equal? a-ship "Battleship")
                      (begin
                        (array-set! (ocean-rep-ships-array an-ocean) row column
                                    (ship row column 4 horizontal #f "B"))
                        (array-set!
                         (ocean-rep-ships-array an-ocean) (+ row 1) column
                         (ship (+ row 1) column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 2) column
                                    (ship (+ row 2) column 4 horizontal #f "B"))
                        (array-set! (ocean-rep-ships-array an-ocean) (+ row 3) column
                                    (ship (+ row 3) column 4 horizontal #f "B")) #t)
                      (if (equal? a-ship "Cruiser")
                          (begin
                            (array-set!
                             (ocean-rep-ships-array an-ocean) row column
                             (ship row column '3 horizontal #f "C"))
                            (array-set!
                             (ocean-rep-ships-array an-ocean) (+ row 1) column
                             (ship (+ row 1) column 3 horizontal #f "C"))
                            (array-set! (ocean-rep-ships-array an-ocean) (+ row 2) column
                                        (ship (+ row 2) column 3 horizontal #f "C")) #t)
                          (if (equal? a-ship "Destroyer")
                              (begin
                                (array-set! (ocean-rep-ships-array an-ocean) row column
                                            (ship row column 2 horizontal #f "D"))
                                (array-set! (ocean-rep-ships-array an-ocean) (+ row 1) column
                                            (ship (+ row 1) column 2 horizontal #f "D")) #t)
                              (if (equal? a-ship "Submarine")
                                  (begin
                                    (array-set! (ocean-rep-ships-array an-ocean) row column
                                                (ship row column 1 horizontal #f "S"))#t)
                                  (void)))))))
            (begin
              (displayln "Illegal placement of the ship.")
              #f))
        (raise "[place-ship-at] an-ocean is not an ocean struct or a-ship is not a ship struct.")))

; check if the ship has been sunk or not by looking at the ship's hits list.
; Return true if it is sunk. Otherwise, false.
(define (is-sunk a-ship)
  (get-hit-of-ship a-ship))

; shoot at a particular location in the ocean
; Return true and mark the corresponding part of the hits list of the ship
; as true if a part of the ship occupies the given row and column and it isn't sunk.
; Otherwise false.
(define (shoot-at row column an-ocean)
  (if (ocean-rep? an-ocean)
      (if (and (< row 10) (>= row 0) (< column 10) (>= column 0))
          (begin
            (let* ([the-target-ship (array-ref (ocean-rep-ships-array an-ocean) row column)]
                   [the-type-of-target (get-ship-type the-target-ship)])
              (if (and (> (ship-length the-target-ship) 0) (not (is-sunk the-target-ship)))
                  (begin
                    (if (equal? the-type-of-target "Battleship")
                        (begin
                          (array-set!
                           (ocean-rep-ships-array an-ocean) row column
                           (ship row column 4 (is-horizontal the-target-ship) #t "H"))
                          #t)
                        (if (equal? the-type-of-target "Cruiser")
                            (begin
                              (array-set! (ocean-rep-ships-array an-ocean) row column
                                          (ship row column 3 (is-horizontal the-target-ship) #t "H"))
                              #t)
                            (if (equal? the-type-of-target "Destroyer")
                                (begin
                                  (array-set! (ocean-rep-ships-array an-ocean) row column
                                              (ship row column 2 (is-horizontal the-target-ship) #t "H"))
                                  #t)
                                (if (equal? the-type-of-target "Submarine")
                                    (begin
                                      (array-set!
                                       (ocean-rep-ships-array an-ocean) row column
                                       (ship row column 1 (is-horizontal the-target-ship) #t "H"))
                                      #t)
                                    (void))))))
                  (if (equal? (ship-display the-target-ship) "H")
                      (begin
                        (displayln "You have attacked that part of the ship already.")
                        #f)
                      (begin
                        (array-set! (ocean-rep-ships-array an-ocean) row column
                                    (ship row column 1 (is-horizontal the-target-ship) #t "X"))
                        #f)))))
          (begin
            (displayln "Illegal coordinate to fire at!")
            #f))
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
  (if (equal? (last player-input-tokens) "horizontal")
      (place-ship-at (string->number (third player-input-tokens)) (string->number (fourth player-input-tokens)) #t ocean (second player-input-tokens))
      (if (equal? (last player-input-tokens) "vertical")
          (place-ship-at (string->number (third player-input-tokens)) (string->number (fourth player-input-tokens)) #f ocean (second player-input-tokens))
          (begin (displayln "Illegal orientation")
                 #f))))

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
  (displayln "To fire at a coordinate, please input \"hit\", row and column ")
  (displayln "If you fire on target, you can see \"H\" marked on the ocean board.")
  (displayln "If you miss the shot, you can see \"X\" marked.")
  (define (run-game)
    (if (is-game-over ocean)
        (raise "You have sunk every single ship. You win!")
        (let* ([player-input (read-line)]
               [player-input-tokens (string-split player-input)])
          (cond [(equal? player-input "")
                 (run-game)]
                [else (if (or (equal? (car player-input-tokens) "exit") (equal? (car player-input-tokens) "quit"))
                          (raise "Player exits the battleship game.")
                          (cond [(> (sum-of-ships-placed num-of-ships-placed-p1) 0)
                                 (cond [(equal? (car player-input-tokens) "place")
                                        ;;;; ships deployment ;;;;
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
                                                                      (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 1 (- (second num-of-ships-placed-p1) 1)))
                                                                      (void))])]
                                                     [(equal? (second player-input-tokens) "Destroyer")
                                                      (cond [(<= (third num-of-ships-placed-p1) 0)
                                                             (displayln "No more Destroyer for deployment.")]
                                                            [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                                                      (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 2 (- (third num-of-ships-placed-p1) 1)))
                                                                      (void))])]
                                                     [(equal? (second player-input-tokens) "Submarine")
                                                      (cond [(<= (fourth num-of-ships-placed-p1) 0)
                                                             (displayln "No more Submarine for deployment.")]
                                                            [else (if (run-game-place-ship-helper player-input-tokens ocean)
                                                                      (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 3 (- (fourth num-of-ships-placed-p1) 1)))
                                                                      (void))])])]
                                              [else (displayln "All ships have been successfully deployed.")])]
                                       [else 
                                        (displayln "Invalid inputs.")])]
                                ; here begins the battle after the ship deployment.
                                [else (displayln "Let's battle begin!")
                                      (cond [(and (equal? (car player-input-tokens) "hit")
                                                  (number? (string->number (second player-input-tokens)))
                                                  (number? (string->number (third player-input-tokens))))
                                             (cond [(shoot-at (string->number (second player-input-tokens)) (string->number (third player-input-tokens)) ocean)
                                                    (set! ocean (struct-copy ocean-rep ocean [ships-sunk (+ (ocean-rep-ships-sunk ocean) 1)]))]
                                                   [else (displayln "You miss the shot!")])]
                                            [else (displayln "Invalid input.")])]))
                      (newline)
                      (displayln "Now your ocean board looks like:")
                      (pretty-print ocean)
                      (run-game)]))))
  (run-game))

(define (a-game-simulation-for-debugging-hit)
  (displayln "The original ocean board")
  (pretty-print ocean)
  (newline)
  (displayln "Let's place 10 ships for debugging hit and attack")
  (place-ship-at 0 0 #t ocean "Battleship")
  (place-ship-at 2 0 #t ocean "Cruiser")
  (place-ship-at 4 0 #t ocean "Cruiser")
  (place-ship-at 6 0 #t ocean "Destroyer")
  (place-ship-at 8 0 #t ocean "Destroyer")
  (place-ship-at 5 5 #t ocean "Destroyer")
  (place-ship-at 0 9 #t ocean "Submarine")
  (place-ship-at 2 9 #t ocean "Submarine")
  (place-ship-at 4 9 #t ocean "Submarine")
  (place-ship-at 6 9 #t ocean "Submarine")
  (newline)
  (displayln "Now the ocean board looks like:")
  (pretty-print ocean)
  (displayln "To fire at a coordinate, please input \"hit\", row and column ")
  (displayln "If you fire on target, you can see \"H\" marked on the ocean board.")
  (displayln "If you miss the shot, you can see \"X\" marked.")
  (define (hit-and-fire)
    (if (not (is-game-over ocean))
        (let* ([player-input (read-line)]
               [player-input-tokens (string-split player-input)])
          (cond [(and (equal? (car player-input-tokens) "hit")
                      (number? (string->number (second player-input-tokens)))
                      (number? (string->number (third player-input-tokens))))
                 (cond [(shoot-at (string->number (second player-input-tokens)) (string->number (third player-input-tokens)) ocean)
                        (set! ocean (struct-copy ocean-rep ocean [ships-sunk (+ (ocean-rep-ships-sunk ocean) 1)]))]
                       [else (displayln "You miss the shot!")])]
                [else (displayln "Invalid input.")])
          (displayln "Now the ocean board looks like:")
          (pretty-print ocean)
          (hit-and-fire))
        (raise "You have sunk every single ship. You win!")))
  (hit-and-fire))

;(game-on)
;(a-game-simulation-for-debugging-hit)

;; Let's start with a basic chat server from Rosetta code: https://rosettacode.org/wiki/Chat_server#Racket
;; This may serve as the foundation of the battleship game
;; I will modify it to be completely different from the original code
;; a game server for two-players battleship game
(define outs (list (current-output-port)))
 
; the main game interface
(define ((player i o))
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
  (displayln "To fire at a coordinate, please input \"hit\", row and column ")
  (displayln "If you fire on target, you can see \"H\" marked on the ocean board.")
  (displayln "If you miss the shot, you can see \"X\" marked.")
  (define (running-the-game i o)
    (let* ([player-input (read-line i)]
           [player-input-tokens (string-split player-input)])
      (if (equal? player-input "")
          (running-the-game i o)
          (if (or (equal? (car player-input-tokens) "exit") (equal? (car player-input-tokens) "quit"))
              (begin
                (raise "Player exits the battleship game")
                (close-output-port o))
              (if (> (sum-of-ships-placed num-of-ships-placed-p1) 0)
                  ; ship deployment
                  (if (not (equal? (car player-input-tokens) "place"))
                      (displayln "Invalid inputs.")
                      (if (equal? (second player-input-tokens) "Battleship")
                          (if (<= (first num-of-ships-placed-p1) 0)
                              (displayln "No more Battleship for deployment.")
                              (if (run-game-place-ship-helper player-input-tokens ocean)
                                  (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 0 (- (first num-of-ships-placed-p1) 1)))
                                  (void)))
                          ; else if it is Cruiser
                          (if (equal? (second player-input-tokens) "Cruiser")
                              (if (<= (second num-of-ships-placed-p1) 0)
                                  (displayln "No more Cruiser for deployment.")
                                  (if (run-game-place-ship-helper player-input-tokens ocean)
                                      (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 1 (- (second num-of-ships-placed-p1) 1)))
                                      (void)))
                              ; else if it is Destroyer
                              (if (equal? (second player-input-tokens) "Destroyer")
                                  (if (<= (third num-of-ships-placed-p1) 0)
                                      (displayln "No more Destroyer for deployment.")
                                      (if (run-game-place-ship-helper player-input-tokens ocean)
                                          (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 2 (- (third num-of-ships-placed-p1) 1)))
                                          (void)))
                                  ; else if it is Submarine
                                  (if (equal? (second player-input-tokens) "Submarine")
                                      (if (<= (fourth num-of-ships-placed-p1) 0)
                                          (displayln "No more Submarine for deployment.")
                                          (if (run-game-place-ship-helper player-input-tokens ocean)
                                              (set! num-of-ships-placed-p1 (list-set num-of-ships-placed-p1 3 (- (fourth num-of-ships-placed-p1) 1)))
                                              (void)))
                                      ; else
                                      (displayln "All ships have been successfully deployed."))))))
                  ; else, that means all ships have been deployed. Start the battle.
                  (begin (displayln "Let's battle begin!")
                         (if (and (equal? (car player-input-tokens) "hit")
                                  (number? (string->number (second player-input-tokens)))
                                  (number? (string->number (third player-input-tokens))))
                             (if (shoot-at (string->number (second player-input-tokens)) (string->number (third player-input-tokens)) ocean)
                                 (set! ocean (struct-copy ocean-rep ocean [ships-sunk (+ (ocean-rep-ships-sunk ocean) 1)]))
                                 (displayln "You miss the shot!"))
                             (displayln "Invalid input."))))))
      (newline)
      (displayln "Now your ocean board looks like:")
      (pretty-print ocean)
      (running-the-game i o)))
  (running-the-game i o))

(define (battleship listener)
  (define-values [i o] (tcp-accept listener))
  (for ([p (list i o)]) (file-stream-buffer-mode p 'none))
  (thread (player i o)) (set! outs (cons o outs)) (battleship listener))
 
(void (thread (λ () (battleship (tcp-listen 8080)))))
((player (current-input-port) (current-output-port)))

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

#lang reader "racets.rkt"
(require racket/stxparam
         web-server/servlet
         web-server/servlet-env
         web-server/dispatch)

; Size of the game board
(define field-size 10)

; Make an empty game board
(define makeboard (lambda () '()))

; Add a piece to the board
(define add-piece (lambda (board x y) (cons (cons x y) board)))

(define add-pieces
  (lambda (board pieces)
    (letrec ([h (lambda (board pieces)
                (if (null? pieces)
                    board
                    (add-pieces (add-piece board (car pieces) (car (cdr pieces)))
                                (cdr (cdr pieces)))))])
    (h board pieces))))

; Play a move on the game board
(define mark-hit
  (lambda (board x y)
    (begin 
      (if (empty? (deref board))
            (cons (deref board) #f)
            (begin
              (if (and (= (car (car (deref board))) x)
                       (= (cdr (car (deref board))) y))
                  (cons (cdr (deref board)) #t)
                  (let ([rst+b (mark-hit (ref (cdr (deref board))) x y)])
                    (let ([ans (cons (cons (car (deref board)) (car rst+b))
                                     (cdr rst+b))])
                      (ref-set! board (car ans))
                      ans))))))))


; Does a given cell exist on the given board
(define has-piece 
  (lambda (board x y)
      (if (null? board)
      #f
      (if (and (= (car (car board)) x)
               (= (cdr (car board)) y))
          #t
          (has-piece (cdr board) x y)))))

; Is a player dead?
(define isdead? (lambda (board) (null? board)))

; Make a policy for a specific player's name
(define mkpol
  (lambda (player-name)
    (let-label l (lambda (x) (equal? x player-name)) l)))

(define (http-response content)  ; The 'content' parameter should be a string.
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
     (string->bytes/utf-8 "<html><head><link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\" integrity=\"sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u\" crossorigin=\"anonymous\"><link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css\" integrity=\"sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp\" crossorigin=\"anonymous\"></head><body style=\"padding-left:10px\">")
     (string->bytes/utf-8 content)
     (string->bytes/utf-8 "</body></html>"))))

(define p1l (mkpol "player1"))
(define p2l (mkpol "player2"))

(define p1board
  (ref (fac p1l
            (add-pieces (makeboard) '(1 2 2 2 3 2 7 7 8 7 9 9 9 8))
            (makeboard))))

(define p2board
  (ref (fac p2l
            (add-pieces (makeboard) '(3 3 3 4 3 5 6 2 7 2 2 6 2 7))
            (makeboard))))

; Observe player 1's board with a specified argument
(define getp1board
  (lambda (arg) (obs p1l arg (deref p1board))))

; Observe player 2's board with a specified argument
(define getp2board
  (lambda (arg) (obs p2l arg (deref p2board))))

; Render Player 1's board to HTML
(define player1board
  (ext-lambda (request name)
              (http-response (string-append 
                              "<h1>Player 1's Game Board</h1>"
                              (pretty-print (getp1board name))))))

; Render Player 2's board to HTML
(define player2board
  (ext-lambda (request name)
              (http-response (string-append 
                              "<h1>Player 2's Game Board</h1>"
                              (pretty-print (getp2board name))))))

(define (char-to-num c) (- (char->integer c) (char->integer #\0)))

(define makep1strike 
  (lambda (x y)
    (begin
      (cdr (obs p2l "player2" (mark-hit p2board x y))))))

; Make a strike from Player 1 to 2
(define p1strike
  (ext-lambda
   (request position)
   (let* ([x (char-to-num (string-ref position 0))]
          [y (char-to-num (string-ref position 2))]
          [ans (makep1strike x y)])
      (http-response 
       (if ans
           "<h1>Congratulations!</h1> <h4>You hit player 2!</h4>"
           "<p>No hit :(</p>")))))

(define makep2strike 
  (lambda (x y)
    (cdr (obs p1l "player1" (mark-hit p1board x y)))))

; Make a strike from Player 2 to 1
(define p2strike
  (ext-lambda
   (request position)
   (let* ([x (char-to-num (string-ref position 0))]
          [y (char-to-num (string-ref position 2))]
          [ans (makep2strike x y)])
     (http-response 
      (if ans
           "<h1>Congratulations!</h1> <h4>You hit player 1!</h4>"
          "<p>No hit :(</p>")))))

; Pretty-print a game board
(define pretty-print 
  (lambda (board)
    (begin 
      (define str "<table class=\"table\">")
      (define (append st) (set! str (string-append str st)))
      (letrec ([per-row-h (lambda (rownum cellnum)
                            (if (> cellnum field-size)
                                (append "</tr>")
                                (begin
                                  (append (if (has-piece board rownum cellnum)
                                              "<td>&#x1F6A2;</td>"
                                              "<td>&#x1F30A;</td>"))
                                  (per-row-h rownum (+ cellnum 1)))))]
               [per-row (lambda (rownum)
                          (begin
                            (append "\n<tr class=\"row-eq-height\">")
                            (per-row-h rownum 0)))]
               [for-all-rows (lambda (rownum)
                               (if (> rownum field-size)
                                   (append "</table>")
                                   (begin (per-row rownum)
                                          (for-all-rows (+ rownum 1)))))])
        (for-all-rows 0)
        str))))

(define-values (dispatch generate-url)
  (dispatch-rules
    [("player1" (string-arg)) player1board]
    [("player2" (string-arg)) player2board]
    [("player1strike" (string-arg)) p1strike]
    [("player2strike" (string-arg)) p2strike]
    ;; [else
    ;;  (http-response "<p>Navigate to /player1/player1 to see player 1's board, or /player2/player2 to see 2's board. Make strikes with /player1strike/x,y or same or 2.</p>")]
    ))

(serve/dispatch dispatch)

#lang racket
(require racket/stxparam
         web-server/servlet
         web-server/servlet-env
         racket/tcp)

;; Let's start with a basic chat server from Rosetta code: https://rosettacode.org/wiki/Chat_server#Racket
;; This may serve as the foundation of the battleship game
;; I will modify it to be completely different from the original code
(define outs (list (current-output-port)))
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
 
(void (thread (λ() (battleship (tcp-listen 8080)))))
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
;; game state
(define (make-state ts fs rs f hs)
  (let ([tableau ts] [foundation fs] [reserve rs] [flower f] [hints hs])
    (lambda (query)
      (case query
        [(tableau) tableau]
        [(foundation) foundation]
        [(reserve) reserve]
        [(flower) flower]
        [(hints) hints]
        [else (error 'game-state "invalid state query" query)]))))

;; deck
(define numeric-offset 48)

(define tableau-length 8)

(define red-char #\R)
(define red-sym #\+)
(define red-hint 0)

(define green-char #\G)
(define green-sym #\-)
(define green-hint 1)

(define black-char #\B)
(define black-sym #\*)
(define black-hint 2)

(define flower-char #\@)
(define flower-sym #\space)

(define (make-card-string sym char) (string sym char sym))

(define (make-ranks schar)
  (append
    (map integer->char (map (lambda (x) (+ x (1+ numeric-offset))) (iota 9)))
    (list schar schar schar schar)))

(define (make-suit sym lst)
  (map (lambda (x) (make-card-string sym x)) lst))

(define deck
  (append
    (make-suit red-sym (make-ranks red-char))
    (make-suit green-sym (make-ranks green-char))
    (make-suit black-sym (make-ranks black-char))
    (list (make-card-string flower-sym flower-char))))

(define (rank card)
  (let ([card-char (string-ref card 1)])
    (and (char-numeric? card-char) (- (char->integer card-char) numeric-offset))))

(define (flower? card) (eq? flower-char (string-ref card 1)))

(define (dragon? card) (not (or (rank card) (flower? card))))

;;new game generation
(define max-seed (1- (expt 2 32)))

(define (new-seed)
  (let ([d (random 1)] [t (random 1)])
    (let ([s (or (and (zero? d) (random (random-seed)))
               (random (time-second (current-time))))])
      (or (and (zero? t) s) (- (max-seed s))))))

(define (list-rotate lst n)
  (let loop ([c n] [l lst])
    (if (zero? c) l (loop (1- c) (append (cdr l) (list (car l)))))))

(define (deal)
  (let loop ([dck deck] [tab (make-list tableau-length '())])
    (if (null? dck)
      (list-rotate tab (random tableau-length))
      (let skip ([tab^ (list-rotate tab (random tableau-length))])
        (if (>= (length (car tab^)) 5)
          (skip (list-rotate tab^ 1))
          (loop (cdr dck)
            (cons (cons (car dck) (car tab^)) (cdr tab^))))))))

(define (make-piles init) (make-list 3 init))

(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles '()) #f (make-piles #f)))

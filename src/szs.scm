;; utility functionality
(define (list-rotate lst n)
  (let loop ([c n] [l lst])
    (if (zero? c) l (loop (1- c) (append (cdr l) (list (car l)))))))

(define (take lst n)
  (let loop ([n n] [xs lst] [ys '()])
    (if (zero? n)
      (reverse ys)
      (loop (1- n) (cdr xs) (cons (car xs) ys)))))

(define (drop lst n)
  (let loop ([n n] [xs lst])
    (if (zero? n)
      xs
      (loop (1- n) (cdr xs)))))

;; deck parameters and operations
(define numeric-offset 48)

(define tableau-width 8)
(define tableau-depth 5)

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

(define (suit card)
  (let ([c (string-ref card 0)])
    (cond
      [(eq? c red-sym) 'red]
      [(eq? c green-sym) 'green]
      [(eq? c black-sym) 'black]
      [else #f])))

(define (flower? card) (eq? flower-char (string-ref card 1)))

(define (dragon? card) (not (or (rank card) (flower? card))))

;; game data structures
(define (make-state ts fs rs f hs)
  (let ([tableau ts] [foundation fs] [reserve rs] [flower f] [hints hs])
    (lambda (query)
      (case query
        [(tableau) tableau]       ; list of list
        [(foundation) foundation] ; list of list
        [(reserve) reserve]       ; list of list
        [(flower) flower]         ; bool
        [(hints) hints]           ; list of bool
        [else (error 'game-state "invalid query" query)]))))

(define (make-selection src-area src-pile ncards dst-area dst-pile)
  (let ([sa src-area] [sp src-pile] [nc ncards] [da dst-area] [dp dst-pile])
    (lambda (query)
      (case query
        [(src-area) sa] ; symbol
        [(src-pile) sp] ; number
        [(ncards) nc]   ; number
        [(dst-area) da] ; symbol
        [(dst-pile) dp] ; number
        [else (error 'selection "invalid query" query)]))))

;; new game generation
(define max-seed (1- (expt 2 32)))

(define (new-seed)
  (let ([d (random 1)] [t (random 1)])
    (let ([s (or (and (zero? d) (random (random-seed)))
               (random (time-second (current-time))))])
      (or (and (zero? t) s) (- (max-seed s))))))

(define (deal)
  (let loop ([dck deck] [tab (make-list tableau-width '())])
    (if (null? dck)
      (list-rotate tab (random tableau-width))
      (let skip ([tab^ (list-rotate tab (random tableau-width))])
        (if (>= (length (car tab^)) tableau-depth)
          (skip (list-rotate tab^ 1))
          (loop (cdr dck)
            (cons (cons (car dck) (car tab^)) (cdr tab^))))))))

(define (make-piles init) (make-list 3 init))

(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles '()) #f (make-piles #f)))

;; game play operations
(define (tableau-move tab src depth dst)
  (let ([selection (take (list-ref tab src) depth)])
    (let loop ([n tableau-width] [newtab '()])
      (if (zero? n)
        newtab
        (let ([next (1- n)])
          (cond
            [(= next dst)
             (loop next (cons (append selection (list-ref tab dst)) newtab))]
            [(= next src)
             (loop next (cons (drop (list-ref tab src) depth) newtab))]
            [else (loop next (cons (list-ref tab next) newtab))]))))))

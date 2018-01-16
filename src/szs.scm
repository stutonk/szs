;; utility functions
;; :: list(a), number -> list(a)
(define (list-rotate lst n)
  (let loop ([c n] [l lst])
    (if (zero? c) l (loop (1- c) (append (cdr l) (list (car l)))))))

;; :: list(a), number -> list(a)
(define (take lst n)
  (let loop ([n n] [xs lst] [ys '()])
    (if (zero? n)
      (reverse ys)
      (loop (1- n) (cdr xs) (cons (car xs) ys)))))

;; :: list(a), number -> list(a)
(define (drop lst n)
  (let loop ([n n] [xs lst])
    (if (zero? n)
      xs
      (loop (1- n) (cdr xs)))))

;; game layout
(define tableau-width 8)
(define tableau-depth 5)

(define reserve-width 3)

(define foundation-width 3)
(define foundation-order '(red green black))

;; :: symbol -> number
(define (place-width place)
  (case place
    [(tableau) tableau-width]
    [(reserve) reserve-width]
    [(foundation) foundation-width]
    [else (error 'place-width "invalid place" place)]))

(define src-areas '(tableau reserve))
(define dst-areas '(tableau reserve foundation))

;; deck parameters and operations
(define numeric-offset 48)

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

;; :: symbol, character -> string
(define (make-card-string sym char) (string sym char sym))

;; :: character -> list(character)
(define (make-ranks schar)
  (append
    (map integer->char (map (lambda (x) (+ x (1+ numeric-offset))) (iota 9)))
    (list schar schar schar schar)))

;; :: symbol, list(character) -> list(string)
(define (make-suit sym lst)
  (map (lambda (x) (make-card-string sym x)) lst))

(define deck
  (append
    (make-suit red-sym (make-ranks red-char))
    (make-suit green-sym (make-ranks green-char))
    (make-suit black-sym (make-ranks black-char))
    (list (make-card-string flower-sym flower-char))))

;; :: string -> number
(define (rank card)
  (let ([card-char (string-ref card 1)])
    (and (char-numeric? card-char) (- (char->integer card-char) numeric-offset))))

;; :: string -> symbol
(define (suit card)
  (let ([c (string-ref card 0)])
    (cond
      [(eq? c red-sym) 'red]
      [(eq? c green-sym) 'green]
      [(eq? c black-sym) 'black]
      [else #f])))

;; :: string -> boolean
(define (flower? card) (eq? flower-char (string-ref card 1)))

;; :: string -> boolean
(define (dragon? card) (not (or (rank card) (flower? card))))

;; game data structures

;; :: list(list(string)), list(list(string)), list(list(string)), boolean, list(boolean) -> closure(game-state)
(define (make-state ts fs rs f hs)
  (let ([tableau ts] [foundation fs] [reserve rs] [flower f] [hints hs])
    (lambda (query)
      (case query
        [(tableau) tableau]
        [(foundation) foundation]
        [(reserve) reserve]
        [(flower) flower]
        [(hints) hints]
        [else (error 'game-state-closure "invalid query" query)]))))

;; :: symbol, number, number, symbol, number -> closure(move)
(define (make-move src-area src-pile ncards dst-area dst-pile)
  (let ([sa src-area] [sp src-pile] [nc ncards] [da dst-area] [dp dst-pile])
    (lambda (query)
      (case query
        [(src-area) sa]
        [(src-pile) sp]
        [(ncards) nc]
        [(dst-area) da]
        [(dst-pile) dp]
        [else (error 'move-closure "invalid query" query)]))))

;; new game generation
(define max-seed (1- (expt 2 32)))

;; :: -> number{1 <= n <= (2^32)-1}
(define (new-seed)
  (let ([d (random 1)] [t (random 1)])
    (let ([s (or (and (zero? d) (random (random-seed)))
               (random (time-second (current-time))))])
      (or (and (zero? t) s) (- (max-seed s))))))

;; :: -> list(list(string))
(define (deal)
  (let loop ([dck deck] [tab (make-list tableau-width '())])
    (if (null? dck)
      (list-rotate tab (random tableau-width))
      (let skip ([tab^ (list-rotate tab (random tableau-width))])
        (if (>= (length (car tab^)) tableau-depth)
          (skip (list-rotate tab^ 1))
          (loop (cdr dck)
            (cons (cons (car dck) (car tab^)) (cdr tab^))))))))

;; :: a -> list(a)
(define (make-piles init) (make-list 3 init))

;; :: -> closure(game-state)
(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles '()) #f (make-piles #f)))

;; game play operations
;; :: closure(game-state), closure(move) -> boolean
(define (valid-tableau-move? state move)
  (let ([tab (state 'tableau)]
         [src (move 'src-pile)]
         [depth (move 'ncards)]
         [dst (move 'dst-pile)])
    (or
      (null? (list-ref tab dst))
      (let ([src-bottom (list-ref (list-ref tab src) depth)]
             [dst-top (car (list-ref tab dst))])
        (not (eq? (suit src-bottom) (suit dst-top)))))))

;; :: closure(game-state), closure(move) -> boolean
(define (valid-reserve-move? state move)
  (let ([ncards (move 'ncards)] [dst (list-ref (state 'reserve) (move 'dst-pile))])
    (and (null? dst) (not (> ncards 1)))))

;; :: closure(game-state), closure(move) -> boolean
(define (valid-foundation-move? state move)
  (let* ([ncards (move 'ncards)]
         [src (list-ref (state (move 'src-area)) (move 'src-pile))]
         [dst-pile (move 'dst-pile)]
         [dst (list-ref (state 'foundation) dst-pile)])
    (and
      (not (> ncards 1))
      (eq? (suit (car src)) (list-ref foundation-order dst-pile))
      (if (null? dst)
        (= 1 (rank (car src)))
        (= (rank (car src)) (1+ (rank (car dst))))))))

;; :: closure(game-state), closure(move) -> boolean
(define (valid-move? state move)
  (let ([src-area (move 'src-area)] [dst-area (move 'dst-area)])
    (cond
      [(and (eq? src-area 'tableau) (eq? dst-area 'tableau))
       (valid-tableau-move? state move)]
      [(eq? dst-area 'reserve) (valid-reserve-move? state move)]
      [(eq? dst-area 'foundation) (valid-foundation-move? state move)]
      [else #f])))

;; :: list(list(string)), number, number, number -> list(list(string))
(define (take-cards place width pile n)
  (let* ([src (list-ref place pile)]
          [taken (take src n)]
          [left (drop src n)])
    (let loop ([c width] [place^ '()])
      (let ([next (1- c)])
        (cond
          [(zero? c) (values taken place^)]
          [(= next pile) (loop next (cons left place^))]
          [else (loop next (cons (list-ref place next) place^))])))))

;; :: list(list(string)), number, number, list(string) -> list(list(string))
(define (give-cards place width pile cards)
  (let loop ([n width] [place^ '()])
    (let ([next (1- n)])
      (cond
        [(zero? n) place^]
        [(= next pile) (loop next (cons (append cards (list-ref place pile)) place^))]
        [else (loop next (cons (list-ref place next) place^))]))))

;; :: closure(game-state), closure(move) -> closure(game-state)
(define (move state move)
  (if (not (valid-move? state move))
    (error 'move "invalid move")
    (let ([sa (move 'src-area)]
           [sp (move 'src-pile)]
           [n (move 'ncards)]
           [da (move 'dst-area)]
           [dp (move 'dst-pile)])
      (let-values ([(cards src^) (take-cards (state sa) (place-width sa) sp n)])
        (let ([dst^ (give-cards (if (eq? sa da) src^ (state da)) (place-width da) dp cards)])
          (let ([areas^ (map (lambda (x) (cond [(eq? da x) dst^]
                                           [(eq? sa x) src^]
                                           [else (state x)]))
                          '(tableau foundation reserve))])
            (make-state (car areas^) (cadr areas^) (caddr areas^) #f (state 'hints))))))))

;; :: closure(game-state) -> boolean
(define (won? state)


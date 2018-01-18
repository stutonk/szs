(import (scheme) (termbox-chez-ffi))
;; utility functions

;; list(a), number -> list(a)
(define (list-rotate lst n)
  (let loop ([c n] [l lst])
    (if (zero? c) l (loop (1- c) (append (cdr l) (list (car l)))))))

;; list(a), number -> list(a)
(define (take lst n)
  (let loop ([n n] [xs lst] [ys '()])
    (if (zero? n)
      (reverse ys)
      (loop (1- n) (cdr xs) (cons (car xs) ys)))))

;; list(a), number -> list(a)
(define (drop lst n)
  (let loop ([n n] [xs lst])
    (if (zero? n)
      xs
      (loop (1- n) (cdr xs)))))

;; (a -> boolean), list(a) -> boolean
(define (every pred lst)
  (if (null? lst) #t (and (pred (car lst)) (every pred (cdr lst)))))

;; game layout
(define tableau-width 8)
(define tableau-depth 5)

(define reserve-width 3)

(define foundation-width 3)
(define foundation-order '(red green black))

;; symbol -> number
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

;; symbol, character -> string
(define (make-card-string sym char) (string sym char sym))

;; character -> list(character)
(define (make-ranks schar)
  (append
    (map integer->char (map (lambda (x) (+ x (1+ numeric-offset))) (iota 9)))
    (list schar schar schar schar)))

;; symbol, list(character) -> list(string)
(define (make-suit sym lst)
  (map (lambda (x) (make-card-string sym x)) lst))

(define deck
  (append
    (make-suit red-sym (make-ranks red-char))
    (make-suit green-sym (make-ranks green-char))
    (make-suit black-sym (make-ranks black-char))
    (list (make-card-string flower-sym flower-char))))

;; string -> number
(define (rank card)
  (let ([card-char (string-ref card 1)])
    (and (char-numeric? card-char) (- (char->integer card-char) numeric-offset))))

;; string -> symbol
(define (suit card)
  (let ([c (string-ref card 0)])
    (cond
      [(eq? c red-sym) 'red]
      [(eq? c green-sym) 'green]
      [(eq? c black-sym) 'black]
      [(eq? (string-ref card 1) flower-char) 'flower]
      [else (error 'suit "invalid card" card)])))

;; string -> boolean
(define (flower? card) (eq? flower-char (string-ref card 1)))

;; string -> boolean
(define (dragon? card) (not (or (rank card) (flower? card))))

;; game data structures

;; list(list(string)), list(list(string)), list(list(string)), boolean, list(boolean)
;;    -> closure(game-state)
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

;; symbol, number, number, symbol, number -> closure(move)
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

;; -> number{1 <= n <= (2^32)-1}
(define (new-seed)
  (let ([d (random 1)] [t (random 1)])
    (let ([s (or (and (zero? d) (random (random-seed)))
               (random (time-second (current-time))))])
      (or (and (zero? t) s) (- (max-seed s))))))

;; -> list(list(string))
(define (deal)
  (let loop ([dck deck] [tab (make-list tableau-width '())])
    (if (null? dck)
      (list-rotate tab (random tableau-width))
      (let skip ([tab^ (list-rotate tab (random tableau-width))])
        (if (>= (length (car tab^)) tableau-depth)
          (skip (list-rotate tab^ 1))
          (loop (cdr dck)
            (cons (cons (car dck) (car tab^)) (cdr tab^))))))))

;; a -> list(a)
(define (make-piles init) (make-list 3 init))

;; -> closure(game-state)
(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles '()) #f (make-piles #f)))

;; game play operations
;; closure(game-state), closure(move) -> boolean
(define (valid-tableau-move? state move)
  (let ([tab (state 'tableau)]
         [src (move 'src-pile)]
         [depth (move 'ncards)]
         [dst (move 'dst-pile)])
    (or
      (null? (list-ref tab dst))
      (let ([src-bottom (list-ref (list-ref tab src) depth)]
             [dst-t (car (list-ref tab dst))])
        (not (eq? (suit src-bottom) (suit dst-t)))))))

;; closure(game-state), closure(move) -> boolean
(define (valid-reserve-move? state move)
  (let ([ncards (move 'ncards)] [dst (list-ref (state 'reserve) (move 'dst-pile))])
    (and (null? dst) (not (> ncards 1)))))

;; closure(game-state), closure(move) -> boolean
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

;; closure(game-state), closure(move) -> boolean
(define (valid-move? state move)
  (let ([src-area (move 'src-area)] [dst-area (move 'dst-area)])
    (cond
      [(and (eq? src-area 'tableau) (eq? dst-area 'tableau))
       (valid-tableau-move? state move)]
      [(eq? dst-area 'reserve) (valid-reserve-move? state move)]
      [(eq? dst-area 'foundation) (valid-foundation-move? state move)]
      [else #f])))

;; list(list(string)), number, number, number -> list(string), list(list(string))
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

;; list(list(string)), number, number, list(string) -> list(list(string))
(define (give-cards place width pile cards)
  (let loop ([n width] [place^ '()])
    (let ([next (1- n)])
      (cond
        [(zero? n) place^]
        [(= next pile) (loop next (cons (append cards (list-ref place pile)) place^))]
        [else (loop next (cons (list-ref place next) place^))]))))

;; closure(game-state), closure(move) -> closure(game-state)
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

;; closure(game-state) -> boolean
(define (won? state) (every (lambda (x) x) (map null? (state 'tableau))))

;; view
(define play-area-width 80)
(define play-area-height 23)

(define color-bg tb-default)
(define color-border (logior tb-white tb-bold))
(define color-card-red (logior tb-red tb-bold))
(define color-card-green tb-green)
(define color-card-black tb-magenta)
(define color-card-flower tb-red)
(define color-card-border (logior tb-white tb-bold))
(define color-empty-place (logior tb-black tb-bold))
(define color-filled-place tb-white)
(define color-place-key (logior tb-cyan tb-bold))

(define (card-color card)
  (case (suit card)
    [(red) color-card-red]
    [(green) color-card-green]
    [(black) color-card-black]
    [(flower) color-card-flower]
    [else (error 'card-color "invalid card" card)]))

(define (display-string str fg bg x0 y)
  (map
    (lambda (c x) (tb-change-cell x y (char->integer c) fg bg))
    (string->list str)
    (map (lambda (n) (+ n x0)) (iota (string-length str)))))

(define (display-border x-offset y-offset)
  (define top y-offset)
  (define bottom (+ y-offset play-area-height -1))
  (define left x-offset)
  (define right (+ x-offset play-area-width -1))
  (define (display-bar lc rc x y)
    (display-string
      (list->string (append `(,lc) (make-list (+ play-area-width -2) #\─) `(,rc)))
      color-border color-bg
      x y))
  (display-bar #\┌ #\┐ left top)
  (display-bar #\└ #\┘ left bottom)
  (let loop ([y (1+ top)])
    (when (< y bottom)
      (tb-change-cell left y (char->integer #\│) color-border color-bg)
      (tb-change-cell right y (char->integer #\│) color-border color-bg)
      (loop (1+ y)))))

(define (display-empty-place color x y)
  (display-string "[   ]" color color-bg x y))

(define (display-empty-hint color x y)
  (display-string "( )" color color-bg x y))

(define (display-place-char c x y)
  (tb-change-cell x y (char->integer c) color-place-key color-bg))

(define place-key-chars '(#\q #\w #\e #\r #\t #\y #\space #\i #\o #\p))
(define tab-key-chars '(#\a #\s #\d #\f #\j #\k #\l #\;))

(define res-cs '((3 2) (10 2) (17 2)))
(define hin-cs '((24 2) (29 2) (34 2)))
(define flo-cs '((45 2)))
(define fou-cs '((58 2) (65 2) (72 2)))
(define tab-cs '((6 6) (15 6) (24 6) (33 6) (42 6) (51 6) (60 6) (69 6)))
(define key-cs
  '((5 1) (12 1) (19 1) (25 1) (30 1) (35 1) (47 1) (60 1) (67 1) (74 1)
    (8 5) (17 5) (26 5) (35 5) (44 5) (53 5) (62 5) (71 5)))

(define (add-offsets cs xo yo)
  (map (lambda (c) (list (+ xo (car c)) (+ yo (cadr c)))) cs))

(define (cons-attr-add-offset as cs xo yo)
  (map (lambda (a c) (cons a c)) as (add-offsets cs xo yo)))

(define (display-key-chars x-offset y-offset)
  (define as (append place-key-chars tab-key-chars))
  (define (dpc c) (display-place-char (car c) (cadr c) (caddr c)))
  (map dpc (cons-attr-add-offset as key-cs x-offset y-offset)))

(define (display-empty-hints x-offset y-offset)
  (define as (list color-card-red color-card-green color-card-black))
  (define (deh c) (display-empty-hint (car c) (cadr c) (caddr c)))
  (map deh (cons-attr-add-offset as hin-cs x-offset y-offset)))

(define (display-empty-places x-offset y-offset)
  (define cs (append res-cs flo-cs fou-cs tab-cs))
  (define as (make-list (length cs) color-empty-place))
  (define (dep c) (display-empty-place (car c) (cadr c) (caddr c)))
  (map dep (cons-attr-add-offset as cs x-offset y-offset)))

(define (display-card card x y)
  (display-string card (card-color card) color-bg x y))

;(define (display-game-state state x-offset y-offset)


(define (display-game-area x-offset y-offset)
  (map
    (lambda (f) (f x-offset y-offset))
    (list display-border display-empty-places display-empty-hints display-key-chars))
    (display-card "+R+" 4 2)
  (tb-present))

(define key-esc 27)

(define (lookup-key evptr)
  (let ([key (ftype-ref tb-event (key) evptr)]
         [ch (ftype-ref tb-event (ch) evptr)])
    (cond
      [(eq? key-esc key) 'quit]
      [else (string->symbol (number->string ch))])))

(define (execute-action continue action)
  (case action
    [(quit) (raise (make-message-condition "Quit game"))]
    [else (continue)]))

(define (main-event-loop evptr)
  (let loop ()
    (let ([ev-type (tb-poll-event evptr)])
      (execute-action
        loop
        (cond
          [(= tb-event-key ev-type) (lookup-key evptr)]
          [(= tb-event-resize ev-type) 'resize]
          [else (loop)])))))

(define (start-game)
  (let ([ev (make-ftype-pointer tb-event (foreign-alloc (ftype-sizeof tb-event)))])
    (define (cleanup)
      (tb-shutdown)
      (foreign-free (ftype-pointer-address ev)))
    (tb-init)
    (with-exception-handler
      (lambda (ex)
        (cleanup)
        (raise ex))
      (lambda ()
        (display-game-area 0 0)
        (main-event-loop ev)))
    (cleanup)))

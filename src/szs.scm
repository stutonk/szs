(import (scheme) (termbox.chezscheme))
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

(define flower (list (make-card-string flower-sym flower-char)))

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
    flower))

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
(define game-width 80)
(define game-height 24)
(define x-offset (make-parameter 0))
(define y-offset (make-parameter 0))

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
(define color-filled-hint tb-yellow)

(define key-esc 27)
(define place-key-chars '(#\q #\w #\e #\r #\t #\y #\space #\i #\o #\p))
(define tab-key-chars '(#\a #\s #\d #\f #\j #\k #\l #\;))
(define empty-place "[   ]")
(define empty-hint "( )")
(define filled-hint-char #\!)

(define res-cs '((3 2) (10 2) (17 2)))
(define hin-cs '((24 2) (29 2) (34 2)))
(define flo-cs '((45 2)))
(define fou-cs '((58 2) (65 2) (72 2)))
(define tab-cs '((6 6) (15 6) (24 6) (33 6) (42 6) (51 6) (60 6) (69 6)))
(define key-cs
  '((5 1) (12 1) (19 1) (25 1) (30 1) (35 1) (47 1) (60 1) (67 1) (74 1)
    (8 5) (17 5) (26 5) (35 5) (44 5) (53 5) (62 5) (71 5)))
(define first-tableau-line 6)
(define max-tableau-height 13)
(define tableau-display-width 68) ; TODO: almost name collision
(define tableau-left 6)

(define (card-color card)
  (case (suit card)
    [(red) color-card-red]
    [(green) color-card-green]
    [(black) color-card-black]
    [(flower) color-card-flower]
    [else (error 'card-color "invalid card" card)]))

(define (add-offsets cs)
  (map (lambda (c) (list (+ (x-offset) (car c)) (+ (y-offset) (cadr c)))) cs))

(define (cons-attr-add-offset as cs)
  (map (lambda (a c) (cons a c)) as (add-offsets cs)))

(define (null-or-card-list lst) (if (null? lst) #f (car lst)))

(define (display-string str fg bg x0 y)
  (map
    (lambda (c x) (tb-change-cell x y (char->integer c) fg bg))
    (string->list str)
    (map (lambda (n) (+ n x0)) (iota (string-length str)))))

(define (display-border)
  (define top (y-offset))
  (define bottom (+ (y-offset) game-height -2))
  (define left (x-offset))
  (define right (+ (x-offset) game-width -1))
  (define (display-bar lc rc x y)
    (display-string
      (list->string (append `(,lc) (make-list (+ game-width -2) #\─) `(,rc)))
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
  (display-string empty-place color color-bg x y))

(define (display-card card x y)
  (display-string card (card-color card) color-bg x y))

(define (display-filled-place card x y)
  (display-empty-place color-filled-place x y)
  (display-card card (1+ x) y))

(define (display-hint color fill? x y)
  (display-string empty-hint color color-bg x y)
  (when fill? (tb-change-cell (1+ x) y filled-hint-char color-filled-hint color-bg)))

(define (display-place-key c x y)
  (tb-change-cell x y (char->integer c) color-place-key color-bg))

(define (display-place-keys)
  (define as (append place-key-chars tab-key-chars))
  (define (dpc c) (display-place-key (car c) (cadr c) (caddr c)))
  (map dpc (cons-attr-add-offset as key-cs)))

(define (display-res/fou r/f cs)
  (map
    (lambda (l)
      (let ([elt (car l)] [x (cadr l)] [y (caddr l)])
        (if elt
          (display-filled-place elt x y)
          (display-empty-place color-empty-place x y))))
    (map (lambda (a d) (cons a d))
      (map null-or-card-list r/f)
      (add-offsets cs))))

(define (display-hints hin)
  (let ([cs (add-offsets hin-cs)])
    (map (lambda (elt)
           (let ([c (car elt)]
                  [f (cadr elt)]
                  [x (caddr elt)]
                  [y (cadddr elt)])
           (display-hint c f x y)))
      (list
        (append `(,color-card-red ,(car hin)) (car cs))
        (append `(,color-card-green ,(cadr hin)) (cadr cs))
        (append `(,color-card-black ,(caddr hin)) (caddr cs))))))

(define (display-flower flo)
  (let* ([cs (add-offsets flo-cs)] [x (caar cs)] [y (cadar cs)])
    (if flo
      (display-filled-place flower x y)
      (display-empty-place color-empty-place x y))))

(define (display-pile pile c)
  (let ([x (car c)] [y0 (cadr c)] [l (length pile)])
    (if (null? pile)
      (display-empty-place color-empty-place x y0)
      (let loop ([n 0] [y y0] [p pile])
        (when (< n l)
          (display-filled-place (car p) x y)
          (loop (1+ n) (1+ y) (cdr p)))))))

(define (display-clear-tableau)
  (define clear-str (make-string tableau-display-width #\space))
  (let ([end (+ first-tableau-line max-tableau-height)])
    (let loop ([y first-tableau-line])
      (when (< y end)
        (display-string clear-str color-bg color-bg tableau-left y)))))

(define (display-tab tab)
  (display-clear-tableau)
  (map display-pile tab (add-offsets tab-cs)))

(define (display-game-state state)
  (let ([res (state 'reserve)]
         [hin (state 'hints)]
         [flo (state 'flower)]
         [fou (state 'foundation)]
         [tab (state 'tableau)])
    (display-res/fou res res-cs)
    (display-hints hin)
    (display-flower flo)
    (display-res/fou fou fou-cs)
    (display-tab tab)
    (tb-present)))

(define (display-static-elements)
  (display-border)
  (display-place-keys)
  (tb-present))

;; controller
(define (lookup-key evptr)
  (let ([key (ftype-ref tb-event (key) evptr)]
         [ch (ftype-ref tb-event (ch) evptr)])
    (cond
      [(eq? key-esc key) 'quit]
      [else (string->symbol (number->string ch))])))

(define (execute-action action)
  (case action
    [(quit) (raise (make-message-condition "Quit game"))]
    [(resize) (resize)]
    [else #f]))

(define (main-event-loop evptr s0)
  (let loop ([states (list s0)])
    (display-game-state (car states))
    (let ([ev-type (tb-poll-event evptr)])
      (execute-action
        (cond
          [(= tb-event-key ev-type) (lookup-key evptr)]
          [(= tb-event-resize ev-type) 'resize]
          [else 'nop])))
    (loop states)))

(define (resize)
  (let ([w (tb-width)] [h (tb-height)])
    (if (or (< w game-width) (< h game-height))
      (error 'resize "game area to small" `(,w ,h))
      (begin
        (x-offset (- (quotient w 2) (quotient game-width 2)))
        (y-offset (- (quotient h 2) (quotient game-height 2)))
        (display-static-elements)))))

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
        (resize)
        (main-event-loop ev (make-new-game))))
    (cleanup)))

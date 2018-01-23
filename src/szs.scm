(import (scheme) (termbox.chezscheme))
;;;; model
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

;; a, list(a) -> number
(define (ffq elt lst)
  (let ([mem? (memq elt lst)])
    (if mem?
      (- (length lst) (length mem?))
      mem?)))

(define tableau-num-piles 8)
(define tabelau-pile-depth 5)

(define reserve-width 3)

(define foundation-width 3)
(define foundation-order '(red green black))

;; symbol -> number
(define (place-width place)
  (case place
    [(tableau) tableau-num-piles]
    [(reserve) reserve-width]
    [(foundation) foundation-width]
    [else (error 'place-width "invalid place" place)]))

(define src-areas '(tableau reserve))
(define dst-areas '(tableau reserve foundation))

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
  (let loop ([dck deck] [tab (make-list tableau-num-piles '())])
    (if (null? dck)
      (list-rotate tab (random tableau-num-piles))
      (let skip ([tab^ (list-rotate tab (random tableau-num-piles))])
        (if (>= (length (car tab^)) tabelau-pile-depth)
          (skip (list-rotate tab^ 1))
          (loop (cdr dck)
            (cons (cons (car dck) (car tab^)) (cdr tab^))))))))

;; a -> list(a)
(define (make-piles init) (make-list 3 init))

;; -> closure(game-state)
(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles '()) #f (make-piles #f)))

(define (compatible-neighbors? src-bottom dst-top)
  (and
    (not (dragon? src-bottom))
    (not (eq? (suit src-bottom) (suit dst-top)))
    (= (1+ (rank src-bottom)) (rank dst-top))))

;; closure(game-state), closure(move) -> boolean
(define (valid-tableau-move? state move)
  (let ([src (list-ref (state (move 'src-area)) (move 'src-pile))]
         [ncards (move 'ncards)]
         [dst (list-ref (state (move 'dst-area)) (move 'dst-pile))])
    (or
      (null? dst)
      (compatible-neighbors? (list-ref src (1- ncards)) (car dst)))))

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
      [(eq? dst-area 'tableau) (valid-tableau-move? state move)]
      [(eq? dst-area 'reserve) (valid-reserve-move? state move)]
      [(eq? dst-area 'foundation) (valid-foundation-move? state move)]
      [else #f])))

;; list(list(string)), number, number, number -> list(string), list(list(string))
(define (take-cards place width pile n)
  (let* ([src (list-ref place pile)]
          [taken (take src n)]
          [left (list-tail src n)])
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
(define (do-move state move)
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
          (make-state (car areas^) (cadr areas^) (caddr areas^) #f (state 'hints)))))))

;; closure(game-state) -> boolean
(define (won? state) (every (lambda (x) x) (map null? (state 'tableau))))

;;;; view
(define game-width 80)
(define game-height 24)
(define x-offset (make-parameter 0))
(define y-offset (make-parameter 0))
(define msg-area-y (1- game-height))
(define first-tableau-line 6)
(define max-tableau-height 13)
(define tableau-char-width 68)
(define tableau-left 6)

(define color-bg tb-black)
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
(define color-warn-fg tb-black)
(define color-warn-bg tb-yellow)
(define color-inform-fg tb-white)
(define color-inform-bg tb-blue)
(define color-error-fg tb-white)
(define color-error-bg tb-red)

(define key-esc 27)
(define place-key-chars '(#\q #\w #\e #\r #\t #\y #\space #\i #\o #\p))
(define tab-key-chars '(#\a #\s #\d #\f #\j #\k #\l #\;))
(define key-chars (append place-key-chars tab-key-chars))
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

(define (top-row-loc ch)
  (let ([i (ffq ch place-key-chars)])
    (case i
      [(0 1 2) (list 'reserve i)]
      [(7 8 9) (list 'foundation (- i 7))]
      [else #f])))

(define (tab-loc ch)
  (list 'tableau (ffq ch tab-key-chars)))

(define (char->location ch)
  (cond
    [(memq ch place-key-chars) (top-row-loc ch)]
    [(memq ch tab-key-chars) (tab-loc ch)]
    [else #f]))

(define (coords area)
  (case area
    [(tableau) tab-cs]
    [(reserve) res-cs]
    [(foundation-width) fou-cs]))

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

(define (display-reverse-place color x y)
  (display-string empty-place color-bg color x y))

(define (display-card card x y)
  (display-string card (card-color card) color-bg x y))

(define (display-reverse-card card x y)
  (let ([color (card-color card)])
    (display-string card color-bg
     (if (eq? color color-card-red) color-card-flower color) x y))) ; suppress blink

(define (display-filled-place card x y)
  (display-empty-place color-filled-place x y)
  (display-card card (1+ x) y))

(define (display-highlighted card x y)
  (display-reverse-place color-filled-place x y)
  (display-reverse-card card (1+ x) y))

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

(define (display-clear-tab)
  (define clear-str (make-string tableau-char-width #\space))
  (let ([end (+ first-tableau-line max-tableau-height)])
    (let loop ([y first-tableau-line])
      (when (< y end)
        (display-string clear-str color-bg color-bg tableau-left y)
        (loop (1+ y))))))

(define (display-tab tab)
  (display-clear-tab)
  (map display-pile (map reverse tab) (add-offsets tab-cs)))

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

(define (highlight-cards state area pile depth)
  (let* ([coord (list-ref (coords area) pile)]
          [ps (list-ref (state area) pile)]
          [offset (1- (length ps))])
    (let ([x (car coord)] [y (+ offset (cadr coord))])
      (let loop ([cards (take ps depth)] [y^ y])
        (when (not (null? cards))
          (display-highlighted (car cards) x y^)
          (loop (cdr cards) (1- y^))))))
  (tb-present))

(define (display-msg msg fg bg)
  (let ([mlen (string-length msg)])
    (when (> mlen game-width)
      (error 'display-msg "message too long" msg))
    (display-string msg fg bg 0 msg-area-y)
    (display-string (make-string (- game-width mlen) #\space) fg bg mlen msg-area-y)
    (tb-present)))

(define (clear-msg) (display-msg "" tb-default tb-default) #t)

(define (error-msg msg) (display-msg msg color-error-fg color-error-bg) #f)

(define (inform-msg msg) (display-msg msg color-inform-fg color-inform-bg) #t)

(define (warn-msg msg) (display-msg msg color-warn-fg color-warn-bg) #t)

(define (warn-ask msg evptr)
  (warn-msg msg)
  (let ([ev (get-next-event evptr)])
    (clear-msg)
    (eq? (ev 'char) #\y)))

(define (resize)
  (let ([w (tb-width)] [h (tb-height)])
    (if (or (< w game-width) (< h game-height))
      (error 'resize "game area to small" `(,w ,h))
      (begin
        (x-offset (- (quotient w 2) (quotient game-width 2)))
        (y-offset (- (quotient h 2) (quotient game-height 2)))
        (display-static-elements)))))

;;;; controller
(define (get-char evptr)
  (integer->char (ftype-ref tb-event (ch) evptr)))

(define (make-event type key char)
  (let ([t type] [k key] [c char])
    (lambda (query)
      (case query
        [(type) t]
        [(key) k]
        [(char) c]))))

(define (get-next-event evptr)
  (let ([ev-type (tb-poll-event evptr)])
    (let ([key (ftype-ref tb-event (key) evptr)]
           [char (get-char evptr)])
      (make-event ev-type key char))))

(define (lookup-keybind event)
  (let ([key (event 'key)] [char (event 'char)])
    (cond
      [(eq? char #\u) 'undo]
      [(eq? char #\z) 'redo]
      [(eq? char #\n) 'new]
      [(eq? key key-esc) 'quit]
      [(memq char (append place-key-chars tab-key-chars)) 'select])))

(define (undo us rs)
  (if (or (null? us) (null? (cdr us)))
    (begin
      (error-msg "Nothing to undo.")
      (values us rs))
    (begin
      (inform-msg (format #f "Move ~a undone." (1- (length us))))
      (values (cdr us) (cons (car us) rs)))))

(define (redo us rs)
  (if (null? rs)
    (begin
      (error-msg "Nothing to redo.")
      (values us rs))
    (begin
      (inform-msg (format #f "Move ~a redone." (length us)))
      (values (cons (car rs) us) (cdr rs)))))

(define (maybe-highlight? state area pile depth)
  (let ([ps (list-ref (state area) pile)])
    (if (>= depth (length ps))
      #f
      (let* ([cards (list-tail ps depth)]
              [curr (car cards)]
              [next (cadr cards)])
        (if (compatible-neighbors? curr next)
          (begin (highlight-cards state area pile (1+ depth)) #t)
          #f)))))

(define (select/move state evptr)
  (let ([loc (char->location (get-char evptr))])
    (if loc
      (let ([sa (car loc)] [sp (cadr loc)])
        (if (null? (list-ref (state sa) sp))
          (error-msg "Location is empty.")
          (begin
            (highlight-cards state sa sp 1)
            (let loop ([d 1])
              (let ([loc^ (char->location ((get-next-event evptr) 'char))])
                (cond
                  [(not loc^) #f]
                  [(equal? loc^ loc) (loop (if (maybe-highlight? state sa sp d) (1+ d) d))]
                  [else (make-move sa sp d (car loc^) (cadr loc^))]))))))
      #f)))

(define (main-event-loop evptr s0)
  (let loop ([us (list s0)] [rs '()])
    (display-game-state (car us))
    (let ([ev (get-next-event evptr)])
      (clear-msg)
      (case (lookup-keybind ev)
        [(undo) (let-values ([(us^ rs^) (undo us rs)]) (loop us^ rs^))]
        [(redo) (let-values ([(us^ rs^) (redo us rs)]) (loop us^ rs^))]
        [(new) (when (warn-ask "Start a new game? (Y/N)" evptr)
                 (loop (list (make-new-game)) '()))]
        [(quit) (when (warn-ask "Quit game? (Y/N)" evptr)
                  (raise (make-message-condition "quit game")))]
        [(select) (let* ([st (car us)] [mv (select/move st evptr)])
                    (when mv
                      (if (not (valid-move? st mv))
                        (error-msg "Invalid move.")
                        (loop (cons (do-move st mv) us) '()))))]
        [else (error-msg "Command not recognized.")]))
    (loop us rs)))

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

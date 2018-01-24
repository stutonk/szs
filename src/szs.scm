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

;; a, list(a) -> number
(define (ffq elt lst)
  (let ([mem? (memq elt lst)])
    (if mem?
      (- (length lst) (length mem?))
      mem?)))

(define (scar p) (if (pair? p) (car p) #f))

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

(define deck
  (cons (cons 'flower 'flower)
    (apply append
      (map
        (lambda (x)
          (map
            (lambda (y) (cons x y))
            (append (map 1+ (iota 9)) (make-list 4 'dragon))))
        '(red green black)))))

(define-record-type state (fields tab res hin flo fou))

(define state-fields '(tab res hin flo fou))

(define (state-sym st sym)
  (case sym
    [(tab) (state-tab st)]
    [(res) (state-res st)]
    [(hin) (state-hin st)]
    [(flo) (state-flo st)]
    [(fou) (state-fou st)]
    [else (error 'state-sym "invalid field symbol" sym)]))

(define-record-type move (fields state from n to bot top))

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

(define (make-new-game)
  (random-seed (new-seed))
    (make-state (deal) (make-piles '()) (make-piles #f) '(()) (make-piles '())))

(define (bottom st loc d)
  (let ([pile (list-ref (state-sym st (car loc)) (cdr loc))])
    (car (reverse (take pile d)))))

(define (top st loc)
  (let ([pile (list-ref (state-sym st (car loc)) (cdr loc))])
    (if (null? pile) '() (car pile))))

(define (compatible-neighbors? b t)
  (and
    (number? (cdr b))
    (number? (cdr t))
    (not (eq? (car b) (car t)))
    (= (1+ (cdr b)) (cdr t))))

(define (valid-tableau-move? mv)
  (or
    (null? (move-top mv))
    (compatible-neighbors? (move-bot mv) (move-top mv))))

(define (valid-reserve-move? mv)
    (and (null? (move-top mv)) (not (> (move-n mv) 1))))

(define (valid-foundation-move? mv)
  (let ([b (move-bot mv)] [t (move-top mv)])
    (and
      (not (> (move-n mv) 1))
      (eq? (car b) (list-ref foundation-order (cdr (move-to mv))))
      (if (null? t)
        (= 1 (cdr b))
        (= (cdr b) (1+ (cdr t)))))))

(define (valid-move? mv)
  (let ([to (car (move-to mv))])
    (cond
      [(eq? to 'tab) (valid-tableau-move? mv)]
      [(eq? to 'res) (valid-reserve-move? mv)]
      [(eq? to 'fou) (valid-foundation-move? mv)]
      [(and (eq? to 'flo) (eq? (car (move-bot mv)) 'flower)) #t]
      [else #f])))

(define (replace-pile area n pile)
  (let loop ([i (1- (length area))] [area^ '()])
    (if (< i 0)
      area^
      (loop (1- i) (cons (if (= i n) pile (list-ref area i)) area^)))))

(define (do-move mv)
  (let* ([st (move-state mv)]
          [from (move-from mv)]
          [to (move-to mv)]
          [n (move-n mv)]
          [src (state-sym st (car from))]
          [taken (take (list-ref src (cdr from)) n)]
          [rem (list-tail (list-ref src (cdr from)) n)]
          [src^ (replace-pile src (cdr from) rem)]
          [dst (if (eq? (car from) (car to)) src^ (state-sym st (car to)))]
          [given (append taken (list-ref dst (cdr to)))]
          [dst^ (replace-pile dst (cdr to) given)])
    (apply make-state (map (lambda (x) (if (symbol? x) (state-sym st x) x))
                        (substq src^ (car from) (substq dst^ (car to) state-fields))))))

(define (won? st) (= tableau-num-piles (length (filter null? (state-tab st)))))

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
(define color-key (logior tb-cyan tb-bold))
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

(define (card-string card)
  (let ([middle (if (number? (cdr card))
                  (number->string (cdr card))
                  (string (char-upcase (string-ref (symbol->string (car card)) 0))))])
    (case (car card)
      [(red) (string-append "+" middle "+")]
      [(green) (string-append "-" middle "-")]
      [(black) (string-append "*" middle "*")]
      [(flower) " @ "])))

(define (card-color card)
  (case (car card)
    [(red) color-card-red]
    [(green) color-card-green]
    [(black) color-card-black]
    [(flower) color-card-flower]
    [else #f]))

(define (top-row-loc ch)
  (let ([i (ffq ch place-key-chars)])
    (case i
      [(0 1 2) (cons 'res i)]
      [(7 8 9) (cons 'fou (- i 7))]
      [else #f])))

(define (tab-loc ch)
  (cons 'tab (ffq ch tab-key-chars)))

(define (char->location ch)
  (cond
    [(memq ch place-key-chars) (top-row-loc ch)]
    [(memq ch tab-key-chars) (tab-loc ch)]
    [else #f]))

(define (coords area)
  (case area
    [(tab) tab-cs]
    [(res) res-cs]
    [(fou) fou-cs]))

(define (add-offsets cs)
  (map (lambda (c) (list (+ (x-offset) (car c)) (+ (y-offset) (cadr c)))) cs))

(define (cons-attr-add-offset as cs)
  (map (lambda (a c) (cons a c)) as (add-offsets cs)))

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

(define (display-card-place x y)
  (display-string empty-place color-empty-place color-bg x y))

(define (display-card card x y)
  (display-string empty-place color-card-border color-bg x y)
  (display-string (card-string card) (card-color card) color-bg (1+ x) y))

(define (display-card-reverse card x y)
  (display-string empty-place color-bg tb-white x y)
  (let ([cc (card-color card)])
    (display-string
      (card-string card) color-bg
      (if (eq? cc color-card-red) tb-red cc)
      (1+ x) y)))

(define (display-hint color fill? x y)
  (display-string empty-hint color color-bg x y)
  (when fill? (display-string (string filled-hint-char) color color-bg (1+ x) y)))

(define (display-key c x y)
  (tb-change-cell x y (char->integer c) color-key color-bg))

(define (display-keys)
  (define (dpc c) (apply display-key c))
  (map dpc (cons-attr-add-offset key-chars key-cs)))

(define (display-res/fou r/f cs)
  (map
    (lambda (l)
      (let ([card (car l)] [x (cadr l)] [y (caddr l)])
        (if card
          (display-card card x y)
          (display-card-place x y))))
    (map cons (map scar r/f) (add-offsets cs))))

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

(define (display-flower flo?)
  (let* ([cs (add-offsets flo-cs)] [x (caar cs)] [y (cadar cs)])
    (if (pair? (car flo?)) (display-card '(flower flower) x y) (display-card-place x y))))

(define (display-pile pile c)
  (let ([x (car c)] [y0 (cadr c)] [l (length pile)])
    (if (null? pile)
      (display-card-place x y0)
      (let loop ([n 0] [y y0] [p pile])
        (when (< n l)
          (display-card (car p) x y)
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

(define (display-state st)
  (let ([res (state-res st)]
         [hin (state-hin st)]
         [flo (state-flo st)]
         [fou (state-fou st)]
         [tab (state-tab st)])
    (display-res/fou res res-cs)
    (display-hints hin)
    (display-flower flo)
    (display-res/fou fou fou-cs)
    (display-tab tab)
    (tb-present)))

(define (display-static-elements)
  (display-border)
  (display-keys)
  (tb-present))

(define (highlight-cards st src pile depth)
  (let* ([coord (list-ref (coords src) pile)]
          [ps (list-ref (state-sym st src) pile)]
          [offset (1- (length ps))])
    (let ([x (car coord)] [y (+ offset (cadr coord))])
      (let loop ([cards (take ps depth)] [y^ y])
        (when (not (null? cards))
          (display-card-reverse (car cards) x y^)
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
    (eq? (event-char ev) #\y)))

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

(define-record-type event (fields type key char))

(define (get-next-event evptr)
  (let ([type (tb-poll-event evptr)])
    (let ([key (ftype-ref tb-event (key) evptr)]
           [char (get-char evptr)])
      (make-event type key char))))

(define (lookup-keybind ev)
  (let ([key (event-key ev)] [char (event-char ev)])
    (cond
      [(eq? char #\u) 'undo]
      [(eq? char #\z) 'redo]
      [(eq? char #\n) 'new]
      [(eq? key key-esc) 'quit]
      [(memq char key-chars) 'select])))

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

(define (maybe-highlight? st src pile depth)
  (let ([ps (list-ref (state-sym st src) pile)])
    (if (>= depth (length ps))
      #f
      (let* ([cards (list-tail ps (1- depth))]
              [curr (car cards)]
              [next (cadr cards)]
              [depth^ (1+ depth)])
        (if (compatible-neighbors? curr next)
          (begin (highlight-cards st src pile depth^) depth^)
          depth)))))

(define (select/move st evptr)
  (let ([loc (char->location (get-char evptr))])
    (if loc
      (let ([src (car loc)] [pile (cdr loc)])
        (if (null? (list-ref (state-sym st src) pile))
          (error-msg "Location is empty.")
          (begin
            (highlight-cards st src pile 1)
            (let loop ([d 1])
              (let ([loc^ (char->location (event-char (get-next-event evptr)))])
                (cond
                  [(not loc^) #f]
                  [(equal? loc^ loc) (loop (maybe-highlight? st src pile d))]
                  [else (make-move st loc d loc^ (bottom st loc d) (top st loc^))]))))))
      #f)))

(define (find-in-loc loc lst t)
  (let ([first
         (scar
           (filter number?
             (map (lambda (x y) (if (and (pair? x) (eq? t (cdr x))) y #f))
               lst (enumerate lst))))])
    (if first (cons loc first) #f)))

(define (find-target st t)
  (let ([tab-tops (map scar (state-tab st))]
         [res-tops (map scar (state-res st))])
    (or (find-in-loc 'res res-tops t) (find-in-loc 'tab tab-tops t))))

(define (min-visible fou)
  (apply min
    (filter number?
      (map (lambda (x) (or (and (pair? x) (cdr x)) 1)) (map scar fou)))))

(define (move-off st src)
  (let* ([src-top (top st src)]
          [dst (case (car src-top)
                 [(red) '(fou . 0)]
                 [(green) '(fou . 1)]
                 [(black) '(fou . 2)]
                 [(flower) '(flo . 0)])])
    (make-move st src 1 dst src-top (top st dst))))

(define (dragons-collectable? st)
  (define (cfilter color) (lambda (x) (and (pair? x) (eq? (car x) color))))
  (let* ([tops (map scar (append (state-res st )(state-tab st)))]
          [ds (filter (lambda (x) (and (pair? x) (eq? (cdr x) 'dragon))) tops)])
    (map (lambda (x) (= 4 (length (filter (cfilter x) ds)))) '(red green black))))

(define (update-hints st hs)
  (make-state (state-tab st) (state-res st) hs (state-flo st) (state-fou st)))

(define (automove st)
  (let ([flower? (and (null? (car (state-flo st))) (find-target st 'flower))]
         [next-target? (find-target st (min-visible (state-fou st)))]
         [ds (dragons-collectable? st)])
    (cond
      [flower? (automove (do-move (move-off st flower?)))]
      [next-target? (automove (do-move (move-off st next-target?)))]
      [(not (equal? ds (state-hin st))) (update-hints st ds)]
      [else st])))

(define (main-event-loop evptr s0)
  (let loop ([us (list (automove s0))] [rs '()])
    (display-state (car us))
    (let ([ev (get-next-event evptr)])
      (clear-msg)
      (case (lookup-keybind ev)
        [(undo) (let-values ([(us^ rs^) (undo us rs)]) (loop us^ rs^))]
        [(redo) (let-values ([(us^ rs^) (redo us rs)]) (loop us^ rs^))]
        [(new) (when (warn-ask "Start a new game? (Y/N)" evptr)
                 (loop (list (make-new-game)) '()))]
        [(quit) (when (warn-ask "Quit game? (Y/N)" evptr)
                  (raise (make-message-condition "quit game")))]
        [(select) (let ([mv (select/move (car us) evptr)])
                    (when mv
                      (if (not (valid-move? mv))
                        (error-msg "Invalid move.")
                        (loop (cons (automove (do-move mv)) us) '()))))]
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

;; TODO: Game shuffling is SHITE
;; TODO: Simplify flo, fou
;; TODO: Decompose into multi files?
;; TODO: Document (at least type sigs)

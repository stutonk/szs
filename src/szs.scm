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
        ;[(type) 'game-state]
        [else (error 'game-state "invalid state query" query)]))))

;; deck
(define numeric-offset 48)

(define red-char #\R)
(define red-sym #\+)

(define green-char #\G)
(define green-sym #\-)

(define black-char #\B)
(define black-sym #\*)

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

(define (dragon? card)
  (let ([card-char (string-ref card 1)])
    (and (not (char-numeric? card-char)) (not (eq? card-char flower-char)))))

(define (rank card)
  (if (dragon? card) #f (- (char->integer (string-ref card 1)) numeric-offset)))

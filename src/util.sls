(library (util)
  (export add1 ffq list-rotate scar sub1 take)
  (import (rnrs))

  ;; number -> number
  (define (add1 n) (+ n 1))

  ;; a, list(a) -> number
  (define (ffq elt lst)
    (let ([mem? (memq elt lst)])
      (if mem?
        (- (length lst) (length mem?))
        mem?)))

  ;; list(a), number -> list(a)
  (define (list-rotate lst n)
    (let loop ([c n] [l lst])
      (if (zero? c) l (loop (sub1 c) (append (cdr l) (list (car l)))))))

  ;; {pair, atom} -> {atom, #f}
  (define (scar p) (if (pair? p) (car p) #f))

  ;; number -> number
  (define (sub1 n) (- n 1))

  ;; list(a), number -> list(a)
  (define (take lst n)
    (let loop ([n n] [xs lst] [ys '()])
      (if (zero? n)
        (reverse ys)
        (loop (sub1 n) (cdr xs) (cons (car xs) ys))))))

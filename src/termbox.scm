(define libdir "/usr/lib/")
(define libfile "libtermbox.so")
(define readlink "readlink -f ")

(define (find-real-path)
  (let ([full-path (string-append libdir libfile)])
    (if (file-symbolic-link? full-path)
      (let ([stdout (car (process (string-append readlink full-path)))])
        (get-line stdout))
      full-path)))

(define (load-lib)
  (load-shared-object (find-real-path)))

(load-lib)

(define tb-init (foreign-procedure "tb_init" () int))

(define tb-shutdown (foreign-procedure "tb_shutdown" () void))

(define tb-clear (foreign-procedure "tb_clear" () void))

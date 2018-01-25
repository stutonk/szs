(load-shared-object "/usr/lib/libtermbox.so.1.0.0")

(define tb-default 0)
(define tb-black 1)
(define tb-red 2)
(define tb-green 3)
(define tb-yellow 4)
(define tb-blue 5)
(define tb-magenta 6)
(define tb-cyan 7)
(define tb-white 8)

(define tb-bold 256)
(define tb-underline 512)
(define tb-reverse 1024)

(define-ftype tb-cell
  (struct
    [ch unsigned-32]
    [fg unsigned-16]
    [bg unsigned-16]))

(define tb-event-error -1)
(define tb-event-key 1)
(define tb-event-mouse 2)
(define tb-event-resize 3)

(define-ftype tb-event
  (struct
    [type unsigned-8]
    [mod unsigned-8]
    [key unsigned-16]
    [ch unsigned-32]
    [w integer-32]
    [h integer-32]
    [x integer-32]
    [y integer-32]))

(define tb-init (foreign-procedure "tb_init" () int))
(define tb-shutdown (foreign-procedure "tb_shutdown" () void))

(define tb-width (foreign-procedure "tb_width" () int))
(define tb-height (foreign-procedure "tb_height" () int))

(define tb-clear (foreign-procedure "tb_clear" () void))
(define tb-set-clear-attributes
  (foreign-procedure "tb_set_clear_attributes" (unsigned-16 unsigned-16) void))

(define tb-present (foreign-procedure "tb_present" () void))

(define tb-set-cursor (foreign-procedure "tb_set_cursor" (int int) void))
(define tb-hide-cursor (lambda () (tb-set-cursor -1 -1)))

(define tb-put-cell (foreign-procedure "tb_put_cell" (int int (* tb-cell)) void))
(define tb-change-cell
  (foreign-procedure "tb_change_cell" (int int unsigned-32 unsigned-16 unsigned-16) void))

(define tb-blit (foreign-procedure "tb_blit" (int int int int (* tb-cell)) void))

(define tb-input-current 0)
(define tb-input-esc 1)
(define tb-input-alt 2)
(define tb-input-mouse 4)

(define tb-select-input-mode
  (foreign-procedure "tb_select_input_mode" (int) void))

(define tb-output-current 0)
(define tb-output-normal 1)
(define tb-output-256 2)
(define tb-output-216 3)
(define tb-output-grayscale 4)

(define tb-select-output-mode
  (foreign-procedure "tb_select_output_mode" (int) int))

(define tb-peek-event
  (foreign-procedure "tb_peek_event" ((* tb-event) int) int))
(define tb-poll-event
  (foreign-procedure "tb_poll_event" ((* tb-event)) int))

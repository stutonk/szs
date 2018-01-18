(library (termbox-chez-ffi)
  (export
    tb-default
    tb-black
    tb-red
    tb-green
    tb-yellow
    tb-blue
    tb-magenta
    tb-cyan
    tb-white

    tb-bold
    tb-underline
    tb-reverse

    tb-event-error
    tb-event-key
    tb-event-mouse
    tb-event-resize

    tb-input-current
    tb-input-esc
    tb-input-alt
    tb-input-mouse

    tb-output-current
    tb-output-normal
    tb-output-256
    tb-output-216
    tb-output-grayscale

    tb-cell
    tb-event

    tb-init
    tb-shutdown
    tb-width
    tb-height
    tb-clear
    tb-set-clear-attributes
    tb-present
    tb-set-cursor
    tb-put-cell
    tb-change-cell
    tb-blit
    tb-select-input-mode
    tb-select-output-mode
    tb-peek-event
    tb-poll-event)
  (import (scheme))

  (define lib-path "/usr/lib/libtermbox.so.1.0.0")

  ;; text colors
  (define tb-default 0)
  (define tb-black 1)
  (define tb-red 2)
  (define tb-green 3)
  (define tb-yellow 4)
  (define tb-blue 5)
  (define tb-magenta 6)
  (define tb-cyan 7)
  (define tb-white 8)

  ;; text attributes
  (define tb-bold 256)
  (define tb-underline 512)
  (define tb-reverse 1024)

  ;; event types
  (define tb-event-error -1)
  (define tb-event-key 1)
  (define tb-event-mouse 2)
  (define tb-event-resize 3)

  ;; input modes
  (define tb-input-current 0)
  (define tb-input-esc 1)
  (define tb-input-alt 2)
  (define tb-input-mouse 4)

  ;; output modes
  (define tb-output-current 0)
  (define tb-output-normal 1)
  (define tb-output-256 2)
  (define tb-output-216 3)
  (define tb-output-grayscale 4)

  ;; foreign data types
  (define-ftype tb-cell
    (struct
      [ch unsigned-32]
      [fg unsigned-16]
      [bg unsigned-16]))

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

  ;; functions
  (define lib-closure
    (let ([_ (load-shared-object lib-path)])
      (let ([tb-init (foreign-procedure "tb_init" () int)]
             [tb-shutdown (foreign-procedure "tb_shutdown" () void)]
             [tb-width (foreign-procedure "tb_width" () int)]
             [tb-height (foreign-procedure "tb_height" () int)]
             [tb-clear (foreign-procedure "tb_clear" () void)]
             [tb-set-clear-attributes
              (foreign-procedure
                "tb_set_clear_attributes"
                (unsigned-16 unsigned-16)
                void)]
             [tb-present (foreign-procedure "tb_present" () void)]
             [tb-set-cursor (foreign-procedure "tb_set_cursor" (int int) void)]
             [tb-put-cell (foreign-procedure "tb_put_cell" (int int (* tb-cell)) void)]
             [tb-change-cell
              (foreign-procedure
                "tb_change_cell"
                (int int unsigned-32 unsigned-16 unsigned-16)
                void)]
             [tb-blit (foreign-procedure "tb_blit" (int int int int (* tb-cell)) void)]
             [tb-select-input-mode
              (foreign-procedure "tb_select_input_mode" (int) void)]
             [tb-select-output-mode
              (foreign-procedure "tb_select_output_mode" (int) int)]
             [tb-peek-event
              (foreign-procedure "tb_peek_event" ((* tb-event) int) int)]
             [tb-poll-event
              (foreign-procedure "tb_poll_event" ((* tb-event)) int)])
      (lambda (sym)
        (case sym
          [(tb-init) tb-init]
          [(tb-shutdown) tb-shutdown]
          [(tb-width) tb-width]
          [(tb-height) tb-height]
          [(tb-clear) tb-clear]
          [(tb-set-clear-attributes) tb-set-clear-attributes]
          [(tb-present) tb-present]
          [(tb-set-cursor) tb-set-cursor]
          [(tb-put-cell) tb-put-cell]
          [(tb-change-cell) tb-change-cell]
          [(tb-blit) tb-blit]
          [(tb-select-input-mode) tb-select-input-mode]
          [(tb-select-output-mode) tb-select-output-mode]
          [(tb-peek-event) tb-peek-event]
          [(tb-poll-event) tb-poll-event])))))

  (define tb-init (lib-closure 'tb-init))
  (define tb-shutdown (lib-closure 'tb-shutdown))
  (define tb-width (lib-closure 'tb-width))
  (define tb-height (lib-closure 'tb-height))
  (define tb-clear (lib-closure 'tb-clear))
  (define tb-set-clear-attributes (lib-closure 'tb-set-clear-attributes))
  (define tb-present (lib-closure 'tb-present))
  (define tb-set-cursor (lib-closure 'tb-set-cursor))
  (define tb-put-cell (lib-closure 'tb-put-cell))
  (define tb-change-cell (lib-closure 'tb-change-cell))
  (define tb-blit (lib-closure 'tb-blit))
  (define tb-select-input-mode (lib-closure 'tb-select-input-mode))
  (define tb-select-output-mode (lib-closure 'tb-select-output-mode))
  (define tb-peek-event (lib-closure 'tb-peek-event))
  (define tb-poll-event (lib-closure 'tb-poll-event)))

;; The following documentation from termbox is included for reference

;/* Key constants. See also struct tb_event's key field.
; *
; * These are a safe subset of terminfo keys, which exist on all popular
; * terminals. Termbox uses only them to stay truly portable.
; */
;#define TB_KEY_F1               (0xFFFF-0)
;#define TB_KEY_F2               (0xFFFF-1)
;#define TB_KEY_F3               (0xFFFF-2)
;#define TB_KEY_F4               (0xFFFF-3)
;#define TB_KEY_F5               (0xFFFF-4)
;#define TB_KEY_F6               (0xFFFF-5)
;#define TB_KEY_F7               (0xFFFF-6)
;#define TB_KEY_F8               (0xFFFF-7)
;#define TB_KEY_F9               (0xFFFF-8)
;#define TB_KEY_F10              (0xFFFF-9)
;#define TB_KEY_F11              (0xFFFF-10)
;#define TB_KEY_F12              (0xFFFF-11)
;#define TB_KEY_INSERT           (0xFFFF-12)
;#define TB_KEY_DELETE           (0xFFFF-13)
;#define TB_KEY_HOME             (0xFFFF-14)
;#define TB_KEY_END              (0xFFFF-15)
;#define TB_KEY_PGUP             (0xFFFF-16)
;#define TB_KEY_PGDN             (0xFFFF-17)
;#define TB_KEY_ARROW_UP         (0xFFFF-18)
;#define TB_KEY_ARROW_DOWN       (0xFFFF-19)
;#define TB_KEY_ARROW_LEFT       (0xFFFF-20)
;#define TB_KEY_ARROW_RIGHT      (0xFFFF-21)
;#define TB_KEY_MOUSE_LEFT       (0xFFFF-22)
;#define TB_KEY_MOUSE_RIGHT      (0xFFFF-23)
;#define TB_KEY_MOUSE_MIDDLE     (0xFFFF-24)
;#define TB_KEY_MOUSE_RELEASE    (0xFFFF-25)
;#define TB_KEY_MOUSE_WHEEL_UP   (0xFFFF-26)
;#define TB_KEY_MOUSE_WHEEL_DOWN (0xFFFF-27)
;
;/* These are all ASCII code points below SPACE character and a BACKSPACE key. */
;#define TB_KEY_CTRL_TILDE       0x00
;#define TB_KEY_CTRL_2           0x00 /* clash with 'CTRL_TILDE' */
;#define TB_KEY_CTRL_A           0x01
;#define TB_KEY_CTRL_B           0x02
;#define TB_KEY_CTRL_C           0x03
;#define TB_KEY_CTRL_D           0x04
;#define TB_KEY_CTRL_E           0x05
;#define TB_KEY_CTRL_F           0x06
;#define TB_KEY_CTRL_G           0x07
;#define TB_KEY_BACKSPACE        0x08
;#define TB_KEY_CTRL_H           0x08 /* clash with 'CTRL_BACKSPACE' */
;#define TB_KEY_TAB              0x09
;#define TB_KEY_CTRL_I           0x09 /* clash with 'TAB' */
;#define TB_KEY_CTRL_J           0x0A
;#define TB_KEY_CTRL_K           0x0B
;#define TB_KEY_CTRL_L           0x0C
;#define TB_KEY_ENTER            0x0D
;#define TB_KEY_CTRL_M           0x0D /* clash with 'ENTER' */
;#define TB_KEY_CTRL_N           0x0E
;#define TB_KEY_CTRL_O           0x0F
;#define TB_KEY_CTRL_P           0x10
;#define TB_KEY_CTRL_Q           0x11
;#define TB_KEY_CTRL_R           0x12
;#define TB_KEY_CTRL_S           0x13
;#define TB_KEY_CTRL_T           0x14
;#define TB_KEY_CTRL_U           0x15
;#define TB_KEY_CTRL_V           0x16
;#define TB_KEY_CTRL_W           0x17
;#define TB_KEY_CTRL_X           0x18
;#define TB_KEY_CTRL_Y           0x19
;#define TB_KEY_CTRL_Z           0x1A
;#define TB_KEY_ESC              0x1B
;#define TB_KEY_CTRL_LSQ_BRACKET 0x1B /* clash with 'ESC' */
;#define TB_KEY_CTRL_3           0x1B /* clash with 'ESC' */
;#define TB_KEY_CTRL_4           0x1C
;#define TB_KEY_CTRL_BACKSLASH   0x1C /* clash with 'CTRL_4' */
;#define TB_KEY_CTRL_5           0x1D
;#define TB_KEY_CTRL_RSQ_BRACKET 0x1D /* clash with 'CTRL_5' */
;#define TB_KEY_CTRL_6           0x1E
;#define TB_KEY_CTRL_7           0x1F
;#define TB_KEY_CTRL_SLASH       0x1F /* clash with 'CTRL_7' */
;#define TB_KEY_CTRL_UNDERSCORE  0x1F /* clash with 'CTRL_7' */
;#define TB_KEY_SPACE            0x20
;#define TB_KEY_BACKSPACE2       0x7F
;#define TB_KEY_CTRL_8           0x7F /* clash with 'BACKSPACE2' */
;
;/* These are non-existing ones.
; *
; * #define TB_KEY_CTRL_1 clash with '1'
; * #define TB_KEY_CTRL_9 clash with '9'
; * #define TB_KEY_CTRL_0 clash with '0'
; */

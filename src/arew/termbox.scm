(library (arew termbox)
  (export
   tb-init
   tb-shutdown
   tb-change-cell
   tb-present
   tb-clear
   tb-height
   tb-width
   tb-set-cursor
   tb-poll-event
   tb-select-output-mode
   tb-event-type

   TB-KEY-F1
   TB-KEY-F2
   TB-KEY-F3
   TB-KEY-F4
   TB-KEY-F5
   TB-KEY-F6
   TB-KEY-F7
   TB-KEY-F8
   TB-KEY-F9
   TB-KEY-F10
   TB-KEY-F11
   TB-KEY-F12
   TB-KEY-INSERT
   TB-KEY-DELETE
   TB-KEY-HOME
   TB-KEY-END
   TB-KEY-PGUP
   TB-KEY-PGDN
   TB-KEY-ARROW-UP
   TB-KEY-ARROW-DOWN
   TB-KEY-ARROW-LEFT
   TB-KEY-ARROW-RIGHT
   TB-KEY-MOUSE-LEFT
   TB-KEY-MOUSE-RIGHT
   TB-KEY-MOUSE-MIDDLE
   TB-KEY-MOUSE-RELEASE
   TB-KEY-MOUSE-WHEEL-UP
   TB-KEY-MOUSE-WHEEL-DOWN

   TB-KEY-CTRL-TILDE
   TB-KEY-CTRL-2
   TB-KEY-CTRL-A
   TB-KEY-CTRL-B
   TB-KEY-CTRL-C
   TB-KEY-CTRL-D
   TB-KEY-CTRL-E
   TB-KEY-CTRL-F
   TB-KEY-CTRL-G
   TB-KEY-BACKSPACE
   TB-KEY-CTRL-H
   TB-KEY-TAB
   TB-KEY-CTRL-I
   TB-KEY-CTRL-J
   TB-KEY-CTRL-K
   TB-KEY-CTRL-L
   TB-KEY-ENTER
   TB-KEY-CTRL-M
   TB-KEY-CTRL-N
   TB-KEY-CTRL-O
   TB-KEY-CTRL-P
   TB-KEY-CTRL-Q
   TB-KEY-CTRL-R
   TB-KEY-CTRL-S
   TB-KEY-CTRL-T
   TB-KEY-CTRL-U
   TB-KEY-CTRL-V
   TB-KEY-CTRL-W
   TB-KEY-CTRL-X
   TB-KEY-CTRL-Y
   TB-KEY-CTRL-Z
   TB-KEY-ESC
   TB-KEY-CTRL-LSQ-BRACKET
   TB-KEY-CTRL-3
   TB-KEY-CTRL-4
   TB-KEY-CTRL-BACKSLASH
   TB-KEY-CTRL-5
   TB-KEY-CTRL-RSQ-BRACKET
   TB-KEY-CTRL-6
   TB-KEY-CTRL-7
   TB-KEY-CTRL-SLASH
   TB-KEY-CTRL-UNDERSCORE
   TB-KEY-SPACE
   TB-KEY-BACKSPACE2
   TB-KEY-CTRL-8

   TB-MOD-ALT
   TB-MOD-MOTION

   TB-DEFAULT
   TB-BLACK
   TB-RED
   TB-GREEN
   TB-YELLOW
   TB-BLUE
   TB-MAGENTA
   TB-CYAN
   TB-WHITE

   TB-BOLD
   TB-UNDERLINE
   TB-REVERSE

   TB-EVENT-KEY
   TB-EVENT-RESIZE
   TB-EVENT-MOUSE

   TB-EUNSUPPORTED-TERMINAL
   TB-EFAILED-TO-OPEN-TTY
   TB-EPIPE-TRAP-ERROR

   TB-DEFAULT
   TB-BLACK
   TB-RED
   TB-GREEN
   TB-YELLOW
   TB-BLUE
   TB-MAGENTA
   TB-CYAN
   TB-WHITE
   )
  (import
   (except (chezscheme) define-record-type)
   (only (scheme base) define-record-type))

  ;; TODO: extract foreign-procedure* outside lambda or define

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define libtermbox (load-shared-object "./local/lib/libtermbox.so"))

    ;; ffi helpers

    (define-syntax-rule (pointer->ftype struct pointer)
      (make-ftype-pointer struct (foreign-ref 'void* pointer 0)))

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure ptr (args ...) return))

    ;; bindings

    (define TB-KEY-F1 (- #xFFFF 0))
    (define TB-KEY-F2 (- #xFFFF 1))
    (define TB-KEY-F3 (- #xFFFF 2))
    (define TB-KEY-F4 (- #xFFFF 3))
    (define TB-KEY-F5 (- #xFFFF 4))
    (define TB-KEY-F6 (- #xFFFF 5))
    (define TB-KEY-F7 (- #xFFFF 6))
    (define TB-KEY-F8 (- #xFFFF 7))
    (define TB-KEY-F9 (- #xFFFF 8))
    (define TB-KEY-F10 (- #xFFFF 9))
    (define TB-KEY-F11 (- #xFFFF 10))
    (define TB-KEY-F12 (- #xFFFF 11))
    (define TB-KEY-INSERT (- #xFFFF 12))
    (define TB-KEY-DELETE (- #xFFFF 13))
    (define TB-KEY-HOME (- #xFFFF 14))
    (define TB-KEY-END (- #xFFFF 15))
    (define TB-KEY-PGUP (- #xFFFF 16))
    (define TB-KEY-PGDN (- #xFFFF 17))
    (define TB-KEY-ARROW-UP (- #xFFFF 18))
    (define TB-KEY-ARROW-DOWN (- #xFFFF 19))
    (define TB-KEY-ARROW-LEFT (- #xFFFF 20))
    (define TB-KEY-ARROW-RIGHT (- #xFFFF 21))
    (define TB-KEY-MOUSE-LEFT (- #xFFFF 22))
    (define TB-KEY-MOUSE-RIGHT (- #xFFFF 23))
    (define TB-KEY-MOUSE-MIDDLE (- #xFFFF 24))
    (define TB-KEY-MOUSE-RELEASE (- #xFFFF 25))
    (define TB-KEY-MOUSE-WHEEL-UP (- #xFFFF 26))
    (define TB-KEY-MOUSE-WHEEL-DOWN (- #xFFFF 27))

    (define TB-KEY-CTRL-TILDE #x00)
    (define TB-KEY-CTRL-2 #x00) ;; clash with CTRL-TILDE
    (define TB-KEY-CTRL-A #x01)
    (define TB-KEY-CTRL-B #x02)
    (define TB-KEY-CTRL-C #x03)
    (define TB-KEY-CTRL-D #x04)
    (define TB-KEY-CTRL-E #x05)
    (define TB-KEY-CTRL-F #x06)
    (define TB-KEY-CTRL-G #x07)
    (define TB-KEY-BACKSPACE #x08)
    (define TB-KEY-CTRL-H #x08) ;; clash with CTRL-BACKSPACE
    (define TB-KEY-TAB #x09)
    (define TB-KEY-CTRL-I #x09) ;; clash with TAB
    (define TB-KEY-CTRL-J #x0A)
    (define TB-KEY-CTRL-K #x0B)
    (define TB-KEY-CTRL-L #x0C)
    (define TB-KEY-ENTER #x0D)
    (define TB-KEY-CTRL-M #x0D) ;; clash with ENTER
    (define TB-KEY-CTRL-N #x0E)
    (define TB-KEY-CTRL-O #x0F)
    (define TB-KEY-CTRL-P #x10)
    (define TB-KEY-CTRL-Q #x11)
    (define TB-KEY-CTRL-R #x12)
    (define TB-KEY-CTRL-S #x13)
    (define TB-KEY-CTRL-T #x14)
    (define TB-KEY-CTRL-U #x15)
    (define TB-KEY-CTRL-V #x16)
    (define TB-KEY-CTRL-W #x17)
    (define TB-KEY-CTRL-X #x18)
    (define TB-KEY-CTRL-Y #x19)
    (define TB-KEY-CTRL-Z #x1A)
    (define TB-KEY-ESC #x1B)
    (define TB-KEY-CTRL-LSQ-BRACKET #x1B) ;; class with ESC
    (define TB-KEY-CTRL-3 #x1B) ;; clash with ESC
    (define TB-KEY-CTRL-4 #x1C)
    (define TB-KEY-CTRL-BACKSLASH #x1C) ;; clash with CTRL-4
    (define TB-KEY-CTRL-5 #x1D)
    (define TB-KEY-CTRL-RSQ-BRACKET #x1D) ;; clash with CTRL-5
    (define TB-KEY-CTRL-6 #x1E)
    (define TB-KEY-CTRL-7 #x1F)
    (define TB-KEY-CTRL-SLASH #x1F) ;; clash with CTRL-7
    (define TB-KEY-CTRL-UNDERSCORE #x1F) ;; clash with CTRL-7
    (define TB-KEY-SPACE #x20)
    (define TB-KEY-BACKSPACE2 #x7F)
    (define TB-KEY-CTRL-8 #x7F) ;; clash with BACKSPACE2

    (define TB-MOD-ALT #x01)
    (define TB-MOD-MOTION #x02)

    (define TB-DEFAULT #x00)
    (define TB-BLACK #x01)
    (define TB-RED #x02)
    (define TB-GREEN #x03)
    (define TB-YELLOW #x04)
    (define TB-BLUE #x05)
    (define TB-MAGENTA #x06)
    (define TB-CYAN #x07)
    (define TB-WHITE #x08)

    (define TB-BOLD #x01000000)
    (define TB-UNDERLINE #x02000000)
    (define TB-REVERSE #x04000000)

    (define TB-EVENT-KEY 1)
    (define TB-EVENT-RESIZE 2)
    (define TB-EVENT-MOUSE 3)

    (define TB-EUNSUPPORTED-TERMINAL -1)
    (define TB-EFAILED-TO-OPEN-TTY -2)
    (define TB-EPIPE-TRAP-ERROR -3)

  (define-syntax switch
    (syntax-rules ()
      [(switch key (value out) ... (else expr))
       (cond
        ((= key value) out)
        ...
        (else expr))]))

  (define-record-type <key>
    (make-key control alternative key)
    key?
    (control key-control)
    (alternative key-alternative)
    (key key-key))

  (define (char-event->key mode key)
    (let ((alternative (= mode TB-MOD-ALT)))
      (switch key
        (TB-KEY-F1 (make-key #f alternative 'F1))
        (TB-KEY-F2 (make-key #f alternative 'F2))
        (TB-KEY-F3 (make-key #f alternative 'F3))
        (TB-KEY-F4 (make-key #f alternative 'F4))
        (TB-KEY-F5 (make-key #f alternative 'F5))
        (TB-KEY-F6 (make-key #f alternative 'F6))
        (TB-KEY-F7 (make-key #f alternative 'F7))
        (TB-KEY-F8 (make-key #f alternative 'F8))
        (TB-KEY-F9 (make-key #f alternative 'F9))
        (TB-KEY-F10 (make-key #f alternative 'F10))
        (TB-KEY-F11 (make-key #f alternative 'F11))
        (TB-KEY-F12 (make-key #f alternative 'F12))
        (TB-KEY-INSERT (make-key #f alternative 'insert))
        (TB-KEY-DELETE (make-key #f alternative 'delete))
        (TB-KEY-HOME (make-key #f alternative 'home))
        (TB-KEY-END (make-key #f alternative 'end))
        (TB-KEY-PGUP (make-key #f alternative 'page-up))
        (TB-KEY-PGDN (make-key #f alternative 'page-down))
        (TB-KEY-ARROW-UP (make-key #f alternative 'arrow-up))
        (TB-KEY-ARROW-DOWN (make-key #f alternative 'arrow-down))
        (TB-KEY-ARROW-LEFT (make-key #f alternative 'arrow-left))
        (TB-KEY-ARROW-RIGHT (make-key #f alternative 'arrow-right))
        (TB-KEY-CTRL-TILDE (make-key #t alternative #\~))
        (TB-KEY-CTRL-2 (make-key #t alternative #\2))
        (TB-KEY-CTRL-A (make-key #t alternative #\a))
        (TB-KEY-CTRL-B (make-key #t alternative #\b))
        (TB-KEY-CTRL-C (make-key #t alternative #\c))
        (TB-KEY-CTRL-D (make-key #t alternative #\d))
        (TB-KEY-CTRL-E (make-key #t alternative #\e))
        (TB-KEY-CTRL-F (make-key #t alternative #\f))
        (TB-KEY-CTRL-G (make-key #t alternative #\g))
        (TB-KEY-BACKSPACE (make-key #t alternative 'backspace))
        (TB-KEY-CTRL-H (make-key #t alternative #\h))
        (TB-KEY-TAB (make-key #t alternative 'tab))
        (TB-KEY-CTRL-I (make-key #t alternative #\i))
        (TB-KEY-CTRL-J (make-key #t alternative #\j))
        (TB-KEY-CTRL-K (make-key #t alternative #\k))
        (TB-KEY-CTRL-L (make-key #t alternative #\l))
        (TB-KEY-ENTER (make-key #f alternative 'enter))
        (TB-KEY-CTRL-M (make-key #t alternative #\m))
        (TB-KEY-CTRL-N (make-key #t alternative #\n))
        (TB-KEY-CTRL-O (make-key #t alternative #\o))
        (TB-KEY-CTRL-P (make-key #t alternative #\p))
        (TB-KEY-CTRL-Q (make-key #t alternative #\q))
        (TB-KEY-CTRL-R (make-key #t alternative #\r))
        (TB-KEY-CTRL-S (make-key #t alternative #\s))
        (TB-KEY-CTRL-T (make-key #t alternative #\t))
        (TB-KEY-CTRL-U (make-key #t alternative #\u))
        (TB-KEY-CTRL-V (make-key #t alternative #\v))
        (TB-KEY-CTRL-W (make-key #t alternative #\w))
        (TB-KEY-CTRL-X (make-key #t alternative #\x))
        (TB-KEY-CTRL-Y (make-key #t alternative #\y))
        (TB-KEY-CTRL-Z (make-key #t alternative #\z))
        (TB-KEY-ESC (make-key #t alternative 'escape))
        ;; not sure what it is and clash with TB-KEY-ESC
        ;; (TB-KEY-CTRL-LSQ-BRACKET (make-key #t alternative 'F1))
        ;; clash with TB-KEY-CTRL-ESC
        ;; (TB-KEY-CTRL-3 (make-key #t alternative '3))
        (TB-KEY-CTRL-4 (make-key #t alternative #\4))
        ;; clash with TB-KEY-CTRL-BACKSLASH
        ;; (TB-KEY-CTRL-BACKSLASH (make-key #t alternative 'F1))
        (TB-KEY-CTRL-5 (make-key #t alternative #\5))
        ;; not sure what it is and clash with TB-KEY-CTRL-5
        ;; (TB-KEY-CTRL-RSQ-BRACKET (make-key #t alternative 'F1))
        (TB-KEY-CTRL-6 (make-key #t alternative #\6))
        (TB-KEY-CTRL-7 (make-key #t alternative #\7))
        ;; not sure what it is and class with TB-KEY-CTRL-7
        ;; (TB-KEY-CTRL-SLASH (make-key #t alternative 'F1))
        ;; (TB-KEY-CTRL-UNDERSCORE (make-key #t alternative 'F1))
        (TB-KEY-SPACE (make-key #f alternative #\space))
        ;; not sure what it is but defined in termbox
        (TB-KEY-BACKSPACE2 (make-key #t alternative 'backspace2))
        ;; clash with TB-KEY-BACKSPACE2
        ;; (TB-KEY-CTRL-8 (make-key #t alternative #\8))
        (else #f))))

    (define (tb-init)
      ((foreign-procedure* int "tb_init")))

    (define (tb-shutdown)
      ((foreign-procedure* void "tb_shutdown")))

    (define (tb-width)
      ((foreign-procedure* int "tb_width")))

    (define (tb-height)
      ((foreign-procedure* int "tb_height")))

    (define (tb-clear)
      ((foreign-procedure* int "tb_clear")))

    (define (tb-set-clear-attributes fg bg)
      ((foreign-procedure* void "tb_set_clear_attributes" unsigned-32 unsigned-32) fg bg))

    (define (tb-present)
      ((foreign-procedure* void "tb_present")))

    (define (tb-set-cursor x y)
      ((foreign-procedure* void "tb_set_cursor" int int) x y))

    (define (tb-hide-cursor)
      (tb-set-cursor -1 -1))

    (define (tb-change-cell x y ch fg bg)
      (let ((proc (foreign-procedure* void
                                      "tb_change_cell"
                                      int
                                      int
                                      unsigned-32
                                      unsigned-32
                                      unsigned-32)))
        (proc x y ch fg bg)))

    (define TB-OUTPUT-CURRENT 0)
    (define TB-OUTPUT-NORMAL 1)
    (define TB-OUTPUT-256 2)
    (define TB-OUTPUT-216 3)
    (define TB-OUTPUT-GRAYSCALE 4)
    (define TB-OUTPUT-TRUECOLOR 5)

    (define (tb-select-output-mode mode)
      ((foreign-procedure* void "tb_select_output_mode" int) mode))

    ;; event

    (define-ftype tb-event
      (struct
       [type integer-8]
       [mod integer-8]
       [key unsigned-16]
       [ch unsigned-32]
       [w integer-32]
       [h integer-32]
       [x integer-32]
       [y integer-32]))

    (define-record-type <event>
      (%make-event type a b c d)
      event?
      (type tb-event-type)
      (a event-a)
      (b event-b)
      (c event-c)
      (d event-d))

    ;; TODO: assume (from srfi-145) on event-type
    (define event-char event-a)

    (define event-key-mod event-a)
    (define event-key-key event-b)

    (define event-resize-width event-a)
    (define event-resize-height event-b)

    (define event-mouse-mod event-a)
    (define event-mouse-key event-b)
    (define event-mouse-x event-c)
    (define event-mouse-y event-d)

    (define (make-event type event)
      (case type
        ((-1) (error 'termbox "event error"))
        ;; TODO: what is this?
        ((0) #f)
        ;; char event
        ((1) (if (= (ftype-ref tb-event (ch) event) 0)
                 (let* ((mode (ftype-ref tb-event (mod) event))
                        (key (char-event->key (ftype-ref tb-event (key) event))))
                   (if (not key)
                       #f
                       (%make-event 'key
                                    key
                                    #f
                                    #f
                                    #f)))
                 (%make-event 'char (ftype-ref tb-event (ch) event) #f #f #f)))
        ;; resize event
        ((2) (%make-event 'resize
                          (ftype-ref tb-event (w) event)
                          (ftype-ref tb-event (h) event)
                          #f
                          #f))
        ;; mouse event
        ((3) (%make-event 'mouse
                          (ftype-ref tb-event (mod) event)
                          (ftype-ref tb-event (key) event)
                          (ftype-ref tb-event (x) event)
                          (ftype-ref tb-event (y) event)))
        (else (error 'termbox "Termbox oops!" type))))

    (define (make-tb-event)
      (make-ftype-pointer tb-event (foreign-alloc (ftype-sizeof tb-event))))

    (define (tb-peek-event timeout)
      (let ((proc (foreign-procedure* int "tb_peek_event" void* int)))
        (let ((event (make-tb-event)))
          (make-event (proc (ftype-pointer-address event) timeout) event))))

    (define (tb-poll-event)
      (let ((proc (foreign-procedure* int "tb_poll_event" void*)))
        (let ((event (make-tb-event)))
          (make-event (proc (ftype-pointer-address event)) event))))

    ))

; tools.scm

;; Chicken 4 got rid of old-style macros; DEFINE-MACRO can still be
;; written like this, however (mostly for legacy code):
(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body ...)
     (define-syntax name
       (lambda (exp ren cmp)
         (apply (lambda (name . args) body ...) exp))))))

(define (make-address hi lo)
  (+ (* hi 256) lo))

(define (high address)
  (bitwise-and (arithmetic-shift (bitwise-and address #xFF00) -8)
               #xFF))

(define (low address)
  (bitwise-and address #xFF))

(define (signed->unsigned x)
  (if (not (<= -128 x 127))
      (error "Invalid value for unsigned byte: " x)
      (if (< x 0)
          (+ 256 x)
          x)))

(define (unsigned->signed x)
  (if (not (<= 0 x 255))
      (error "Invalid value for signed byte: " x)
      (if (>= x 128)
          (- x 256)
          x)))

(define (make-relative address curr-address)
  ;; Make the 16-bit address relative to curr_address, returning the
  ;; relative value as an unsigned byte.
  (let ((delta (- address curr-address)))
    (if (or (< delta -128)
            (> delta 127))
        (error "Branching out of bounds: " delta) ; fixme
        delta)))

(define (make-absolute relative curr-address)
  (+ curr-address relative))

;;
;; stuff that should be in R5RS or a SRFI but isn't.

(use srfi-13)

;; replace all occurrences of <before> in s with <after>.
;; NOTE: may cause infinite loop if <before> is a substring of <after>!
(define (string-subst s before after)
  ;; check if <before> is a substring of <s>
  (let ((idx (string-contains s before))
        (after-length (string-length after))
        (before-length (string-length before)))
    (if idx
        (let ((new-s (string-replace s after idx (+ idx before-length)
                                     0 after-length)))
          (string-subst new-s before after)) ; look if we can subst more
        s))) ; done!

;;
;; slicing, sort of like Python

(define (slice s start end)
  (let* ((len (string-length s))
         (start (if (negative? start) (+ len start) start))
         (end (if (negative? end) (+ len end) end)))
    (substring s start end)))

;; catch exception and return a default value
(define-macro (exception-default default . body)
  (let ((exn (gensym)))
    `(handle-exceptions ,exn ,default ,@body)))

;; take at most N items from LST.  if LST has less than N items, return
;; those, rather than raising an error.
(define (take-at-most lst n)
  (define (take-at-most-aux lst n acc)
    (if (or (null? lst) (= n 0))
        (reverse acc)
        (take-at-most-aux (cdr lst) (- n 1) (cons (car lst) acc))))
  (take-at-most-aux lst n '()))

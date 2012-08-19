;; sasm.scm
;; S-expr notation for 6502 assembler.
;;
;; SASM provides a notation and is able to translate SASM exprs to strings,
;; but it is *not* a disassembler, and does not look up labels or verify if
;; they exist.

(use srfi-13 format)
(load-relative "opcodes")

(define (sasm-translate-label sasm-expr)
  (sprintf "~a:" (second sasm-expr)))

(define (sasm-string-repr sasm-expr opcode)
  (let* ((name (string-upcase (symbol->string (opcode-name opcode))))
         (operand
          (cond ((= (opcode-size opcode) 1) "")
                ((< (length sasm-expr) 3)
                 (error "Invalid length: " sasm-expr))
                ((symbol? (third sasm-expr))
                 (format (opcode-pattern-with-label opcode) (third sasm-expr)))
                ((or (= (opcode-size opcode) 3)
                     (branch-opcode? (opcode-value opcode)))
                 (let* ((n (third sasm-expr))
                        (fmt (opcode-pattern opcode)))
                   (string-upcase (format fmt (high n) (low n)))))
                ((= (opcode-size opcode) 2)
                 (string-upcase
                  (format (opcode-pattern opcode) (third sasm-expr))))
                (else (error "Invalid expr: " sasm-expr)))))
    (string-trim-both (sprintf "~a ~a" name operand))))

(define (sasm-translate-opcode sasm-expr)
  (let* ((wanted-name (first sasm-expr))
         ; try to get the opcode type from the second element (if any)
         (wanted-type-1 (find-opcode-alias
                         (if (null? (cdr sasm-expr)) #f (second sasm-expr))))
         ; if that failed, try to determine the opcode type through other means
         (wanted-type (or wanted-type-1
                          (determine-opcode-type wanted-name)
                          (error "Could not determine opcode type:" sasm-expr)))
         (wanted? (lambda (opcode)
                    (and (equal? wanted-name (opcode-name opcode))
                         (equal? wanted-type (opcode-type opcode)))))
         (opcode (find wanted? *opcodes*)))
    (if opcode
        (if wanted-type-1
            ; if the opcode type was specified, pass it as-is
            (sasm-string-repr sasm-expr opcode)
            ; otherwise, add the opcode type we found to the sasm-expr
            (let ((fixed-sasm-expr
                   (cons (car sasm-expr) (cons wanted-type (cdr sasm-expr)))))
              (sasm-string-repr fixed-sasm-expr opcode)))
        (error "Opcode not found: " sasm-expr))))

(define (sasm-translate-data sasm-expr)
  (define (data-as-string x)
    (cond
     ((string? x) (format "~s" x))
     ((number? x) (format "~2,'0X" x))
     (else (error "Unknown data element: " x))))
  (let* ((data (map data-as-string (cdr sasm-expr)))
         (s (string-join data " ")))
    (string-append "DATA " s)))

(define (sasm-translate-call sasm-expr)
  (let* ((call (cadr sasm-expr)))
    (eval call)))
;; XXX in order for the eval to work, the name of the function being called
;; (the "macro", although it's not a macro to Scheme) needs to be accessible.

(define (sasm-translate sasm-expr)
  (cond ((pair? sasm-expr)
         (case (car sasm-expr)
           ((label) (sasm-translate-label sasm-expr))
           ((data)  (sasm-translate-data sasm-expr))
           ((call)  (sasm-translate-call sasm-expr))
           (else    (sasm-translate-opcode sasm-expr))))
        ((symbol? sasm-expr)
         (sasm-translate-label (list 'label sasm-expr)))
        (else (error "Invalid SASM expression:" sasm-expr))))

(define-macro (sasm . body)
  (list 'flatten (list 'map 'sasm-translate (list 'quasiquote body))))

;; define-sasm-macro:
;; Shorthand for (define (foo bar) (sasm ...))
;; Allows us to say e.g.
;; (define-sasm-macro (chrout c)
;;   (lda im ,c)
;;   (jsr a #xFFD2))
;;
(define-macro (define-sasm-macro header . body)
  `(define ,header (sasm ,@body)))

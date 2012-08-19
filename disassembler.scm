;; disassembler.scm

(use format)
(use srfi-13)
(load-relative "opcodes")
(load-relative "tools")

;; too complex; refactor
(define (disassemble-codes codes acc address)
  (if (null? codes)
      (reverse acc)
      (let* ((byte-value (first codes))
             (opcode (exception-default #f (find-opcode byte-value))))
        (if opcode
            (let* ((size (opcode-size opcode))
                   (curr-address (+ address size))
                   (rest (reverse (cdr (take-at-most codes size))))) ; lsb first
              (if (< (length rest) (- size 1))
                  (reverse acc) ; FIXME: process invalid opcodes at end
                  (begin
                    (when (branch-opcode? byte-value)
                      (set! rest (branch-rest rest curr-address)))
                    (let* ((line (make-line opcode rest address))
                           (codes-left (drop codes size)))
                      (disassemble-codes codes-left (cons line acc)
                                         curr-address)))))
            ; opcode is invalid:
            (let ((line (make-line-for-invalid-opcode byte-value address)))
              (disassemble-codes (cdr codes) (cons line acc) (+ address 1)))))))

;; branching opcodes: convert the byte indicating the jump (relative) to an
;; absolute address (two bytes).
(define (branch-rest rest curr-address)
  (let* ((relative (unsigned->signed (first rest)))
         (target (make-absolute relative curr-address)))
    (list (high target) (low target))))

;; create an assembler line of the form "address opcode [operands]"
;; e.g. "C000 ADC $25", etc
(define (make-line opcode bytes curr-address)
  (let* ((pattern (opcode-pattern opcode))
         (name-part (symbol->string (opcode-name opcode)))
         (operand-part (apply format pattern bytes))
         (address-part (format #f "~4,'0X" curr-address))
         (raw-line (conc address-part " " name-part " " operand-part)))
    (string-trim-both (string-upcase raw-line))))

(define (make-line-for-invalid-opcode value address)
  (string-upcase (format "~4,'0X ?? ($~2,'0X)" address value)))

(define (read-bytes port)
  (let ((all-chars (string->list (read-string #f port))))
    (map char->integer all-chars)))

;; --- main program ---

(define (main args)
  (let* ((source-filename (first args)))
    (with-input-from-file source-filename
      (lambda ()
        (let* ((bytes (read-bytes (current-input-port)))
               (first-two (take bytes 2))
               (lo (first first-two))
               (hi (second first-two))
               (start-address (make-address hi lo))
               (lines (disassemble-codes (drop bytes 2) '() start-address)))
          (for-each print lines))))))

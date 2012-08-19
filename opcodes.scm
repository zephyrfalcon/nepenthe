;; opcodes.scm

(use srfi-1)
(load-relative "tools")

(define *opcodes* '(
    (adc immediate #x69 2)
    (adc zeropage #x65 2)
    (adc zeropage-x #x75 2)
    (adc absolute #x6D 3)
    (adc absolute-x #x7D 3)
    (adc absolute-y #x79 3)
    (adc indirect-x #x61 2)
    (adc indirect-y #x71 2)

    (and immediate #x29 2)
    (and zeropage #x25 2)
    (and zeropage-x #x35 2)
    (and absolute #x2D 3)
    (and absolute-x #x3D 3)
    (and absolute-y #x39 3)
    (and indirect-x #x21 2)
    (and indirect-y #x31 2)

    (asl implied #x0A 1)
    (asl zeropage #x06 2)
    (asl zeropage-x #x16 2)
    (asl absolute #x0E 3)
    (asl absolute-x #x1E 3)

    (bcc relative #x90 2)

    (bcs relative #xB0 2)

    (beq relative #xF0 2)

    (bit zeropage #x24 2)
    (bit absolute #x2C 3)

    (bmi relative #x30 2)

    (bne relative #xD0 2)

    (bpl relative #x10 2)

    (brk implied #x00 1)

    (bvc relative #x50 2)

    (bvs relative #x70 2)

    (clc implied #x18 1)

    (cld implied #xD8 1)

    (cli implied #x58 1)

    (clv implied #x38 1)

    (cmp immediate #xC9 2)
    (cmp zeropage #xC5 2)
    (cmp zeropage-x #xD5 2)
    (cmp absolute #xCD 3)
    (cmp absolute-x #xDD 3)
    (cmp absolute-y #xD9 3)
    (cmp indirect-x #xC1 2)
    (cmp indirect-y #xD1 2)

    (cpx immediate #xE0 2)
    (cpx zeropage #xE4 2)
    (cpx absolute #xEC 3)

    (cpy immediate #xC0 2)
    (cpy zeropage #xC4 2)
    (cpy absolute #xCC 3)

    (dec zeropage #xC6 2)
    (dec zeropage-x #xD6 2)
    (dec absolute #xCE 3)
    (dec absolute-x #xDE 3)

    (dex implied #xCA 1)

    (dey implied #x88 1)

    (eor immediate #x49 2)
    (eor zeropage #x45 2)
    (eor zeropage-x #x55 2)
    (eor absolute #x4D 3)
    (eor absolute-x #x5D 3)
    (eor absolute-y #x59 3)
    (eor indirect-x #x41 2)
    (eor indirect-y #x51 2)

    (inc zeropage #xe6 2)
    (inc zeropage-x #xF6 2)
    (inc absolute #xEE 3)
    (inc absolute-x #xFE 3)

    (inx implied #xE8 1)

    (iny implied #xC8 1)

    (jmp absolute #x4C 3)
    (jmp indirect #x6C 3)

    (jsr absolute #x20 3)

    (lda immediate #xA9 2)
    (lda zeropage #xA5 2)
    (lda zeropage-x #xB5 2)
    (lda absolute #xAD 3)
    (lda absolute-x #xBD 3)
    (lda indirect-x #xA1 2)
    (lda indirect-y #xB1 2)

    (ldx immediate #xA2 2)
    (ldx zeropage #xA6 2)
    (ldx zeropage-y #xB6 2)
    (ldx absolute #xAE 3)
    (ldx absolute-y #xBE 3)

    (ldy immediate #xA0 2)
    (ldy zeropage #xA4 2)
    (ldy zeropage-x #xB4 2)
    (ldy absolute #xAC 3)
    (ldy absolute-x #xBC 3)

    (lsr implied #x4A 1)
    (lsr zeropage #x46 2)
    (lsr zeropage-x #x56 2)
    (lsr absolute #x4E 3)
    (lsr absolute-x #x5E 3)

    (nop implied #xEA 1)

    (ora immediate #x09 2)
    (ora zeropage #x05 2)
    (ora zeropage-x #x15 2)
    (ora absolute #x0D 3)
    (ora absolute-x #x1D 3)
    (ora absolute-y #x19 3)
    (ora indirect-x #x01 2)
    (ora indirect-y #x11 2)

    (pha implied #x48 1)

    (php implied #x08 1)

    (pla implied #x68 1)

    (plp implied #x28 1)

    (rol implied #x2A 1)
    (rol zeropage #x26 2)
    (rol zeropage-x #x36 2)
    (rol absolute #x2E 3)
    (rol absolute-x #x35 3)

    (ror implied #x6A 1)
    (ror zeropage #x66 2)
    (ror zeropage-x #x76 2)
    (ror absolute #x6E 3)
    (ror absolute-x #x7E 3)

    (rti implied #x40 1)

    (rts implied #x60 1)

    (sbc immediate #xE9 2)
    (sbc zeropage #xE5 2)
    (sbc zeropage-x #xF5 2)
    (sbc absolute #xED 3)
    (sbc absolute-x #xFD 3)
    (sbc absolute-y #xF9 3)
    (sbc indirect-x #xE1 2)
    (sbc indirect-y #xF1 2)

    (sec implied #x38 1)

    (sed implied #xF8 1)

    (sei implied #x78 1)

    (sta zeropage #x85 2)
    (sta zeropage-x #x95 2)
    (sta absolute #x8D 3)
    (sta absolute-x #x9D 3)
    (sta absolute-y #x99 3)
    (sta indirect-x #x81 2)
    (sta indirect-y #x91 2)

    (stx zeropage #x86 2)
    (stx zeropage-y #x96 2)
    (stx absolute #x8E 3)

    (sty zeropage #x84 2)
    (sty zeropage-x #x94 2)
    (sty absolute #x8C 3)

    (tax implied #xAA 1)

    (tay implied #xA8 1)

    (tsx implied #xBA 1)

    (txa implied #x8A 1)

    (txs implied #x9A 1)

    (tya implied #x98 1)
))

(define (opcode-name opcode) (list-ref opcode 0))
(define (opcode-type opcode) (list-ref opcode 1))
(define (opcode-value opcode) (list-ref opcode 2))
(define (opcode-size opcode) (list-ref opcode 3))

(define (_opcode-regex-base_ opcode-type)
  "Return the regular expression (as a string) that matches the given opcode."
  (case opcode-type
    ((immediate) "#(&H2)")
    ((zeropage) "(&H2)")
    ((zeropage-x) "(&H2)\\s*,\\s*X")
    ((zeropage-y) "(&H2)\\s*,\\s*Y")
    ((absolute relative) "(&H4|&L)")
    ((absolute-x) "(&H4|&L)\\s*,\\s*X")
    ((absolute-y) "(&H4|&L)\\s*,\\s*Y")
    ((indirect) "\\(\\s*(&H4|&L)\\s*\\)")
    ((indirect-x) "\\(\\s*(&H2)\\s*,\\s*X\\s*\\)")
    ((indirect-y) "\\(\\s*(&H2)\\s*\\)\\s*,\\s*Y")
    ((implied) "^\\s*([A-Z]{3})\\s*()$")
    (else (error "Unknown type: " opcode-type))))

(define *opcode-types*
  '(immediate zeropage zeropage-x zeropage-y absolute absolute-x absolute-y 
    relative indirect indirect-x indirect-y implied))

(define (_convert-regex-base_ opcode-type regex-base)
  (let* ((s1 (string-subst regex-base "&H2" "\\$[0-9A-F]{2}"))
         (s2 (string-subst s1 "&H4" "\\$[0-9A-F]{4}"))
         (s3 (string-subst s2 "&L" "[A-Z_][A-Z0-9_]*")))
    (if (equal? opcode-type 'implied)
        s3 ; leave as-is
        (string-append "^\\s*([A-Z]{3})\\s+" s3 "\\s*$"))))

(define (_convert-regex-base-aux_ opcode-type)
  (let ((regex-base (_opcode-regex-base_ opcode-type)))
    (list opcode-type (_convert-regex-base_ opcode-type regex-base))))
(define *opcode-regexen*
  (map _convert-regex-base-aux_ *opcode-types*))

(define (opcode-regex opcode)
  (let ((pair (assoc (opcode-type opcode) *opcode-regexen*)))
    (if pair
        (cadr pair)
        (error "Invalid opcode: " (opcode-type opcode)))))

(define (opcode-pattern opcode)
  (case (opcode-type opcode)
    ((immediate) "#$~2,'0X")
    ((zeropage) "$~2,'0X")
    ((zeropage-x) "$~2,'0X,X")
    ((zeropage-y) "$~2,'0X,Y")
    ((absolute relative) "$~2,'0X~2,'0X")
    ((absolute-x) "$~2,'0X~2,'0X,X")
    ((absolute-y) "$~2,'0X~2,'0X,Y")
    ((indirect) "($~2,'0X~2,'0X)")
    ((indirect-x) "($~2,'0X,X)")
    ((indirect-y) "($~2,'0X),Y")
    ((implied) "")
    (else (error "Unknown type: " (opcode-type opcode)))))

(define (opcode-pattern-with-label opcode)
  (case (opcode-type opcode)
    ((absolute relative) "~a")
    ((absolute-x) "~a,X")
    ((absolute-y) "~a,Y")
    ((indirect) "(~a)")
    (else (error "Opcode does not support labels: " opcode))))

(define *branch-opcodes* '(#x10 #x30 #x50 #x70 #x90 #xB0 #xD0 #xF0)) 

(define (branch-opcode? byte-value)
  (member byte-value *branch-opcodes*))

(define (find-opcode byte-value)
  (define (wanted? o) (equal? (opcode-value o) byte-value))
  (let ((opcode (find wanted? *opcodes*)))
    (or opcode
        (error "Could not find opcode with value: " byte-value))))

;; aliases for opcodes

(define *opcode-aliases*
  '((immediate im)
    (zeropage z zero)
    (zeropage-x zx)
    (zeropage-y zy)
    (absolute a abs)
    (relative r rel)
    (absolute-x ax)
    (absolute-y ay)
    (indirect i id ind)
    (indirect-x ix)
    (indirect-y iy)
    (implied imp)))

;; XXX rewrite using 'find'
(define (find-opcode-alias opcode-type)
  (define (find-opcode-alias-aux aliases)
    (if (null? aliases)
        #f
        (if (member opcode-type (car aliases))
            (caar aliases)
            (find-opcode-alias-aux (cdr aliases)))))
  (find-opcode-alias-aux *opcode-aliases*))

;; try to determine the opcode type for the given name; return #f upon failure
;; for whatever reason (ambiguity, opcode dinnae exist, etc).
(define (determine-opcode-type name)
  (let ((opcodes (filter (lambda (op) (equal? name (opcode-name op)))
                         *opcodes*)))
    (if (= (length opcodes) 1)
        (opcode-type (car opcodes))
        #f)))

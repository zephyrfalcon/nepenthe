;; test-assembler.scm

(use test)
(load "assembler")

(test-group "recognition"
  (test #t (label? "BEGIN:"))
  (test #f (label? "NOT A LABEL"))
  (test #f (label? "23"))
  (test #t (label? "CBM-CHROUT:"))

  (test #t (data? "DATA 01 02 03"))
  (test #t (data? "DATA \"HELLO\""))
  (test #f (data? "LDA #$04"))
)

(test-group "extract-label-name"
  (test "BEGIN" (extract-label-name "  BEGIN:  "))
)

(test-group "register-label"
  (define state (make-assembler-state))
  ;; add two labels
  (register-label state "L1")
  (register-label state "L2")
  ;; make sure both labels are present
  (test 2 (length (assembler-state-labels state)))
  (test-assert (assoc "l1" (assembler-state-labels state)))
  (test-assert (assoc "l2" (assembler-state-labels state)))
  ;; assert that adding L1 again is an error
  (test-error (register-label state "L1"))
)

(test-group "extract-data-bytes"
  (test '((#x20) "21 22") (extract-data-bytes "20 21 22"))
  (test '((#x40) "") (extract-data-bytes "40"))
  (test #f (extract-data-bytes "bah humbug"))
)

(test-group "extract-data-string"
  (test '((72 69 76 76 79) "") (extract-data-string "\"HELLO\"  "))
  (test '(() "") (extract-data-string "\"\""))
  (test #f (extract-data-string "bah humbug"))
  (test #f (extract-data-string ""))
)

(test-group "parse-data"
  (test '(65 66 67) (parse-data "DATA \"ABC\"  "))
  (test '(65 32 66 32 67 #xA3 #x20) (parse-data "DATA \"A B C\" A3 20"))
)

(test-group "write-program"
  (define state (make-assembler-state))
  (assembler-state-start-address-set! state #xC000)
  (write-program state "bogus" '(65 66 67 34))
  ;; TODO: test if file really exists
)

(test-group "get-opcode-name"
  (test "LDA" (get-opcode-name "LDA #$04"))
  (test "NOP" (get-opcode-name "NOP"))
  (test-error (get-opcode-name "123"))
)

(test-group "get-opcodes-with-name"
  (test 7 (length (get-opcodes-with-name "LDA")))
  (test 1 (length (get-opcodes-with-name "PHP")))
  (test 0 (length (get-opcodes-with-name "XYZ")))
)

(test-group "find-matching-opcode"
  (let* ((opcodes (get-opcodes-with-name "LDA"))
         (result (find-matching-opcode "LDA #$03" opcodes))
         (opcode (first result))
         (match (second result)))
    (test 'lda (opcode-name opcode))
    (test #xA9 (opcode-value opcode))
    (test "LDA" (first match))
    (test "$03" (second match))) ; notice the $ is included

  (let* ((opcodes (get-opcodes-with-name "BRK"))
         (result (find-matching-opcode "BRK" opcodes))
         (opcode (first result))
         (match (second result)))
    (test 'brk (opcode-name opcode))
    (test #x00 (opcode-value opcode))
    (test "BRK" (first match)))

  ; test LDA with label
  (let* ((opcodes (get-opcodes-with-name "LDA"))
         (result (find-matching-opcode "LDA AARDAPPEL" opcodes))
         (opcode (first result))
         (match (second result)))
    (test 'lda (opcode-name opcode))
    (test #xAD (opcode-value opcode))
    (test "LDA" (first match))
    (test "AARDAPPEL" (second match)))
)

(test-group "get-values-from-operand"
  (test '(#x03) (get-values-from-operand "$03"))
  (test '(#x34 #x12) (get-values-from-operand "$1234"))
)

(test-group "lookup-label"
  (define state (make-assembler-state))
  (assembler-state-current-address-set! state #xC000)
  (register-label state "AARDAPPEL")
  ;(printf "~s~n" (assembler-state-labels state))
  (test #xC000 (lookup-label state "AARDAPPEL" #t))
  (test #xC000 (lookup-label state "aardappel" #t))
  (test "bloemkool" (lookup-label state "BLOEMKOOL" #f))
  (test-error (lookup-label state "BLOEMKOOL" #t))
)

(test-group "add-labels"
  (load "kernal")
  (define state (make-assembler-state))
  (assembler-state-current-address-set! state #xC000)
  (register-label state "TOMAAT")
  (add-labels state *kernal-addresses*)
  (test 4 (length (assembler-state-labels state)))
  (test #xFFD2 (lookup-label state "CBM-CHROUT" #t))
)

;;
;; --- high-level tests ---

(test-group "assemble-line"
  (define state (make-assembler-state))
  (define data 
    '(("ADC #$03"       (#x69 #x03))
      ("  ADC  $14  "   (#x65 #x14))
      ("ADC $C000"      (#x6D #x00 #xC0))
      ("ADC $C000,X"    (#x7D #x00 #xC0))
      ("ADC ($DD,X)"    (#x61 #xDD))
      ("ADC ($EE),Y"    (#x71 #xEE))
      ("ASL"            (#x0A))
      ("ASL $E0"        (#x06 #xE0))
      ("ASL $C000"      (#x0E #x00 #xC0))
      ("JMP ($C000)"    (#x6C #x00 #xC0))
      ("BRK"            (#x00))
      ("JMP HELLO"      (#x4C "HELLO"))
      ("BNE BEGIN_LOOP" (#xD0 "BEGIN_LOOP"))
      ("DOERAK:"        ())))
  (define (test-foo item)
    (let* ((line (first item))
           (expected (second item)))
      (test line expected (assemble-line state line))))
  (for-each test-foo data)

  (test-assert (lookup-label state "DOERAK" #t))
)

(test-group "assembler-pass-1 and 2"
  (let* ((state (make-assembler-state))
         (data '("LDA $0380"
                 "LDX $0381"
                 "STA $0381"
                 "STX $0380"
                 "  BRK  "))
         (expected '(#xAD #x80 #x03 #xAE #x81 #x03
                     #x8D #x81 #x03 #x8E #x80 #x03 #x00)))
    (load-assembler-code state data)
    (test expected (assembler-pass-1 state))
    (test expected (assembler-pass-2 state expected)))
)
         
(test-group "assemble"
  ;; test code with one label (that is known right from the start)
  (let* ((state (make-assembler-state))
         (data "BEGIN:
                  LDA #$08
                  STA $0381
                  JMP BEGIN")
         (expected '(#xA9 #x08 #x8D #x81 #x03 #x4C #x00 #xC0))
         (lines (string-split data "\n")))
    (load-assembler-code state lines)
    (test expected (assemble state 
                             start-address: #xC000 current-address: #xC000)))

  ;; test code with label that is undefined at some point
  (let* ((state (make-assembler-state))
         (data "ONE:
                  LDA #$08
                  STA $0381
                  JMP TWO
                  BRK
                TWO:
                  LDA #$09
                  PHP
                  RTS")
         (expected '(#xA9 #x08 #x8D #x81 #x03 #x4C #x09 #xC0
                     #x00 #xA9 #x09 #x08 #x60))
         (lines (string-split data "\n")))
    (load-assembler-code state lines)
    (test expected (assemble state start-address: #xC000
                             current-address: #xC000)))

  ;; test code with branching opcode (jumping backward)
  (let* ((state (make-assembler-state))
         (data "LABEL1:
                  LDA $9000,X
                  INX
                  CMP #$00
                  BNE LABEL1
                  RTS")
         (expected '(#xBD #x00 #x90 #xE8 #xC9 #x00 #xD0 #xF8 #x60))
         (lines (string-split data "\n")))
    (load-assembler-code state lines)
    (test expected (assemble state start-address: #xC000
                             current-address: #xC000)))

  ;; test code with branching opcode (jumping forward)
  (let* ((state (make-assembler-state))
         (data "  CMP $D010           ; compare A with whatever is at $D010
                  BNE LABEL2          ; if not equal, go to LABEL2
                  RTS
                LABEL2:
                  JSR $E200           ; call whatever subroutine is at $E200")
         (expected '(#xCD #x10 #xD0 #xD0 #x01 #x60 #x20 #x00 #xE2))
         (lines (string-split data "\n")))
    (load-assembler-code state lines)
    (test expected (assemble state start-address: #x1000
                             current-address: #x1000)))

)

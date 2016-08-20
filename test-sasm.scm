;; test-sasm.scm

(use test)
(load "sasm")

(test-group "sasm-translate"
  ;; labels
  (test "foo:" (sasm-translate '(label foo)))

  ;; data
  (test "DATA \"hello\"" (sasm-translate '(data "hello")))
  (test "DATA 42 44 \"0\"" (sasm-translate '(data #x42 #x44 "0")))

  ;; opcodes
  (test "PHP" (sasm-translate '(php implied)))
  (test "LDA #$04" (sasm-translate '(lda im #x04)))
  (test "BNE hello" (sasm-translate '(bne rel hello)))
  (test "BEQ $C000" (sasm-translate '(beq rel #xC000)))
  (test "AND $12" (sasm-translate '(and z #x12)))
  (test "ASL $13,X" (sasm-translate '(asl zx #x13)))
  (test "LDX $14,Y" (sasm-translate '(ldx zy #x14)))
  (test "LDY $C000" (sasm-translate '(ldy a #xC000)))
  (test "LDA $C000,X" (sasm-translate '(lda ax #xC000)))
  (test "LDX $C000,Y" (sasm-translate '(ldx ay #xC000)))
  (test "LDA GREETING,X" (sasm-translate '(lda ax GREETING)))
  (test "JMP ($D000)" (sasm-translate '(jmp ind #xD000)))
  (test "SBC ($D4,X)" (sasm-translate '(sbc ix #xD4)))
  (test "SBC ($D4),Y" (sasm-translate '(sbc iy #xD4)))

  ;; TODO: cover sasm-exprs without type qualifiers! bcc, jsr, etc
  (test "RTS" (sasm-translate '(rts)))
  (test "JSR $C000" (sasm-translate '(jsr #xC000)))
  (test "BNE BEGIN_LOOP" (sasm-translate '(bne BEGIN_LOOP)))
  (test-error (sams-translate '(lda #x04)))

  ;; single symbols should be treated as labels
  (test "XYZZY:" (sasm-translate 'XYZZY))
)

(test-group "sasm"
  (define a (sasm (php implied)
                  (lda im #x04)
                  (bne rel hello)))
  (define b '("PHP" "LDA #$04" "BNE hello"))
  (test b (identity a))

  ;; define a function that uses sasm, returning a list of strings
  (define (chrout c)
    (sasm
     (lda im ,c)
     (jsr a #xFFD2)))
  (test '("LDA #$20" "JSR $FFD2") (chrout 32))

  ;; use define-sasm-macro for the same purpose
  (define-sasm-macro (chrout2 c)
    (lda im ,c)
    (jsr a #xFFD2))
  (test '("LDA #$20" "JSR $FFD2") (chrout2 32))
)

;; this needs to be global, otherwise CALL in SASM-TRANSLATE won't "see" it
(define-sasm-macro (global-chrout c)
  (lda im ,c)
  (jsr a #xFFD2))

(test-group "sasm-translate-and-macros"
  (test '("LDA #$20" "JSR $FFD2") (sasm-translate '(call (global-chrout 32))))
  (define blurb
    (sasm
     (ldx im 32)
     (call (global-chrout 13))))
  (printf "blurb:~n~s~n" blurb)
  (test-assert (equal? '("LDX #$20" "LDA #$0D" "JSR $FFD2") blurb))
)

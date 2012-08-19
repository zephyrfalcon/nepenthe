;; helloworld-3a.scm
;; Produces helloworld.prg.
;; LOAD "HELLOWORLD.PRG",8,1
;; SYS 49152

(load-relative "../assembler")
(load-relative "../sasm")

(assemble-and-write "helloworld.prg" #xC000 #xC000
  (string-split "
        LDX #$00                ; set counter to 0
    BEGIN_LOOP:
        LDA GREETING,X
        JSR $FFD2               ; output character
        INX
        CMP #$00                ; does A contain #$00?
        BNE BEGIN_LOOP
        RTS

    GREETING:
        DATA \"HELLO WORLD\" 00
    " "\n"))
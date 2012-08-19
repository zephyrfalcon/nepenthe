;; helloworld.asm

    LDX #$00                ; set counter to 0
BEGIN_LOOP:
    LDA GREETING,X
    JSR $FFD2               ; output character
    INX
    CMP #$00                ; does A contain #$00?
    BNE BEGIN_LOOP
    RTS

GREETING:
    DATA "HELLO WORLD" 00


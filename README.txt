# README.txt

Nepenthe is a cross-assembler suite written in Chicken Scheme, providing:

- a 6502 assembler [assembler.scm]
- a 6502 disassembler [disassembler.scm]
- a library of useful functions for the Commodore 64 and Commodore 16
  [to be announced]

By extension, processors that derived from the 6502 are also
supported, esp. the 6510 (used in the C-64) and the 8501 (used in the
C-16 and Plus/4).

In addition to "regular" 6502 assembler notation, Nepenthe also
provides SASM, or "Scheme Assembler", a way to write assembler code as
S-expressions.

--- Requirements ---

Nepenthe is written in Chicken Scheme.  The version currently being
used for development is 4.6.0.  Older versions may or may not work.

See: http://www.call-with-current-continuation.org/

A number of "eggs" is required, viz.
- args
- format
- regex
- srfi-1
- srfi-13
- test (required to run the test suite, but not for the actual (dis)assembler)

Some of these are built-in into Chicken and do not need to be
downloaded separately.


--- assembler.scm ---


--- disassembler.scm ---


--- SASM ---

The sasm macro supports any number of sasm-expressions, which may have
the following forms:

    (<opcode> <type> [operand])
    (label <name>)
    (data ...strings and numbers...)

For example:

    (lda im #x04)              ; LDA #$04
    (label FOO)                ; FOO:
    (data "hello" #x0F)        ; DATA "HELLO" 0F

As sasm-expressions don't support different syntaxes like "ADC #$20",
"ADC $20,X", "ADC $3000,X" etc, we need to indicate the type of the
opcode.  The following types are supported (aliases in parentheses):

     immediate    (im)
     zeropage     (z, zero)
     zeropage-x   (zx)
     zeropage-y   (zy)
     absolute     (a, abs)
     relative     (r, rel)
     absolute-x   (ax)
     absolute-y   (ay)
     indirect     (i, id, ind)
     indirect-x   (ix)
     indirect-y   (iy)
     implied      (imp)

So, (ldy a #xC000) translates to "LDY $C000", but (ldy ax #xC000) is
"LDY $C000,X", etc.

If there is no ambiguity, the type indicator can be omitted; for
example, the following sasm-expressions are all equivalent:

   (rts immediate)
   (rts im)
   (rts)

Also, if a sasm-expression consists of a single symbol, it is
considered to be a label; again, the following expressions are
equivalent:

   BAR
   (label BAR)


;; macros-1.scm

(load-relative "../assembler")
(load-relative "../sasm")

(define-sasm-macro (chrout c)
  (lda im ,c)
  (jsr #xFFD2))

;; SYS49152; writes "ABC" to the screen
(assemble-and-write "macros1.prg" #xC000 #xC000
  (sasm
   (call (chrout 65))
   (call (chrout 66))
   (call (chrout 67))
   (call (chrout 13))
   (rts)))

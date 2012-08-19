;; helloworld-3.scm
;; Produces helloworld.prg.
;; LOAD "HELLOWORLD.PRG",8,1
;; SYS 49152

(load-relative "../assembler")
(load-relative "../sasm")

(assemble-and-write "helloworld.prg" #xC000 #xC000
  (sasm
   (ldx im #x00)
   BEGIN_LOOP
   (lda ax GREETING)
   (jsr #xFFD2)
   (inx)
   (cmp im #x00)
   (bne BEGIN_LOOP)
   (rts)

   GREETING
   (data "HELLO WORLD" #x00)))

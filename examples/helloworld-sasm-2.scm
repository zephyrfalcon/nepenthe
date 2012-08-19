;; helloworld-sasm-2.scm
;; Like helloworld-sasm.scm, but with shorter SASM expressions. ^_^

(load-relative "../sasm")

(define hello-world
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

(for-each print hello-world)

(load-relative "../assembler.scm")

(define state (make-assembler-state))
(load-assembler-code state hello-world)
(define bytes (assemble state start-address: #xC000 current-address: #xC000))
(write-program state "helloworld-2.prg" bytes)

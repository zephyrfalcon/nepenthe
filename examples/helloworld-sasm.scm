;; helloworld-sasm.scm

(load-relative "../sasm")

(define hello-world
  (sasm
   (ldx im #x00)
   (label BEGIN_LOOP)
   (lda ax GREETING)
   (jsr a #xFFD2)
   (inx imp)
   (cmp im #x00)
   (bne rel BEGIN_LOOP)
   (rts imp)

   (label GREETING)
   (data "HELLO WORLD" #x00)))

(for-each print hello-world)

(load-relative "../assembler.scm")

(define state (make-assembler-state))
(load-assembler-code state hello-world)
(define bytes (assemble state start-address: #xC000 current-address: #xC000))
(write-program state "helloworld-2.prg" bytes)

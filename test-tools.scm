;; test-tools.scm

(use test)
(load "tools")

(test-group "make-address"
  (test #xC000 (make-address #xC0 #x00))
  (test #x1234 (make-address #x12 #x34)))

(test-group "high"
  (test #x08 (high #x0801))
  (test #xC0 (high #xC000))
  (test #x12 (high #x1234))
)

(test-group "low"
  (test #x01 (low #x0801))
  (test #x00 (low #xC000))
  (test #x34 (low #x1234))
)

(test-group "signed->unsigned"
  (define s->u signed->unsigned)
  (test 0 (s->u 0))
  (test 20 (s->u 20))
  (test #xFF (s->u -1))
  (test #xF7 (s->u -9))
  (test #x80 (s->u -128))
  (test-error (s->u 200))
)

(test-group "unsigned->signed"
  (define u->s unsigned->signed)
  (test 0 (u->s 0))
  (test 20 (u->s 20))
  (test -128 (u->s #x80))
  (test -1 (u->s #xFF))
  (test -9 (u->s #xF7))
  (test-error (u->s -1))
)

(test-group "slicing"
  (test "ell" (slice "Hello" 1 -1))
)

;; test-opcodes.scm

(use test)
(load "opcodes")

(test-group "*opcodes*"
  (define (test-opcode opcode)
    (define test-name (sprintf "test opcode: ~s" opcode))
    (test-assert test-name (member (opcode-type opcode) *opcode-types*)))
  (for-each test-opcode *opcodes*))

(test-group "branch-opcode?"
  (test-assert (branch-opcode? #x30))
  (test #f (branch-opcode? #x20)))

(test-group "find-opcode"
  (define a (find-opcode #x98))
  (test 'tya (opcode-name a))
  (test 'implied (opcode-type a))
)

(test-group "find-opcode-alias"
  (test 'immediate (find-opcode-alias 'im))
  (test 'zeropage-x (find-opcode-alias 'zx))
  (test 'absolute (find-opcode-alias 'absolute))
  ;(test-error (find-opcode-alias 'bogus)))
  (test #f (find-opcode-alias 'bogus))
)

(test-group "determine-opcode-type"
  (test 'implied (determine-opcode-type 'php))
  (test 'implied (determine-opcode-type 'rts))
  (test 'absolute (determine-opcode-type 'jsr))
  (test #f (determine-opcode-type 'adc))
)


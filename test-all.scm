;; test-all.scm

;; Just load the test-*.scm files here.

(use test)

;; set parameters
(current-test-verbosity #f)

(load "test-tools")
(load "test-opcodes")
(load "test-assembler")
(load "test-disassembler")
(load "test-sasm")
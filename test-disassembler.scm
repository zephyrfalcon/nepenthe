;; test-disassembler.scm

(use test)
(load-relative "disassembler")

(test-group "make-line"
  (define adc-im (find-opcode #x69))
  (test "C000 ADC #$32" (make-line adc-im '(#x32) #xC000))

  (let ((and-z (find-opcode #x25)))
    (test "D000 AND $41" (make-line and-z '(#x41) #xD000)))

  (let ((asl-zx (find-opcode #x16)))
    (test "E100 ASL $33,X" (make-line asl-zx '(#x33) #xE100)))

  (let ((ldx-zy (find-opcode #xB6)))
    (test "A000 LDX $20,Y" (make-line ldx-zy '(#x20) #xA000)))

  (let ((bcc (find-opcode #x90)))
    (test "F000 BCC $F010" (make-line bcc '(#xF0 #x10) #xF000)))

  (let ((cmp-a (find-opcode #xCD)))
    (test "0800 CMP $1234" (make-line cmp-a '(#x12 #x34) #x0800)))

  (let ((cmp-ax (find-opcode #xDD)))
    (test "1000 CMP $2345,X" (make-line cmp-ax '(#x23 #x45) #x1000)))

  (let ((cmp-ay (find-opcode #xD9)))
    (test "1000 CMP $2345,Y" (make-line cmp-ay '(#x23 #x45) #x1000)))

  (let ((dex (find-opcode #xCA)))
    (test "2000 DEX" (make-line dex '() #x2000)))

  (let ((jmp-i (find-opcode #x6C)))
    (test "D000 JMP ($C000)" (make-line jmp-i '(#xC0 #x00) #xD000)))

  (let ((eor-ix (find-opcode #x41)))
    (test "2800 EOR ($17,X)" (make-line eor-ix '(#x17) #x2800)))

  (let ((eor-iy (find-opcode #x51)))
    (test "3000 EOR ($19),Y" (make-line eor-iy '(#x19) #x3000)))

)

(test-group "disassemble-codes"
  (test '("C000 NOP") (disassemble-codes '(#xEA) '() #xC000))

  (test '("C000 NOP"
          "C001 NOP"
          "C002 NOP")
        (disassemble-codes '(#xEA #xEA #xEA) '() #xC000))

  (test '("C000 JMP $D000")
        (disassemble-codes '(#x4C #x00 #xD0) '() #xC000))

  ;; TODO: more tests!!!
  ;; also test non-assembler byte values

  (test '("C000 ?? ($FF)"
          "C001 ?? ($FF)")
        (disassemble-codes '(#xFF #xFF) '() #xC000))
)
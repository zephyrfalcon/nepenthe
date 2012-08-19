;; assembler.scm

(use args)
(use srfi-1)
(use srfi-13)
(use regex)
(load-relative "opcodes")
(load-relative "tools")

;;
;; --- assembler state ---

(define-record assembler-state
  labels            ; assoc-list with pairs ("name" address)
  source-lines      ; assembler source as a list of lines (strings)
  start-address     ; starting address for assembling
  current-address)  ; current address while assembling

(define _make-assembler-state_ make-assembler-state)
(define (make-assembler-state)
  (_make-assembler-state_ '() '() 0 0))

;;
;; --- loading assembler code ---

(define (load-assembler-code state source-lines)

  (define (non-empty? line)
    (> (string-length (string-trim-both line)) 0))

  (define (remove-comment line)
    (let ((semicolon-pos (string-contains line ";")))
      (if semicolon-pos
          (string-take line semicolon-pos)
          line)))

  (let* ((lines1 (filter non-empty? source-lines))
         (lines2 (map remove-comment lines1))
         (lines3 (map string-trim-both lines2)))
    (assembler-state-source-lines-set! state lines3)
    state))

;;
;; --- labels ---

;; Labels are stored in an assoc-list with pairs (name address).  They are
;; case-insensitive strings; before being registered, they are converted to
;; lowercase.

(define label-regex (regexp "^\\s*([A-Z_][A-Z_0-9-]*):\\s*$"))

(define (label? line)
  (and (string-match label-regex line) #t))

(define (extract-label-name line)
  (second (string-match label-regex line)))

(define (register-label state label-name)
  (let ((label-name (string-downcase label-name)))
    ;; if the label already exists, raise an error
    (if (assoc label-name (assembler-state-labels state))
        (error "Label already exists: " label-name)
        (let* ((addr (assembler-state-current-address state))
               (labels (assembler-state-labels state))
               (new-labels (cons (list label-name addr) labels)))
          (assembler-state-labels-set! state new-labels)))))

;; load a collection of labels and corresponding address (as seen in e.g.
;; kernal.scm)
(define (add-labels state labels)
  (let* ((current-labels (assembler-state-labels state))
         (new-labels (map (lambda (pair)
                            (list (string-downcase (first pair)) (second pair)))
                          labels))
         (all-labels (append current-labels new-labels)))
    (assembler-state-labels-set! state all-labels)))

;; look up the given label name in <state>.  if found, return the
;; corresponding address.  otherwise, if strict, raise an error; if
;; non-strict, return the label name as-is.
;;
(define (lookup-label state name strict?)
  (let* ((labels (assembler-state-labels state))
         (name (string-downcase name))
         (pair (assoc name labels))) ; (name address) or #f
    (if pair
        (second pair) ; return the address
        (if strict?
            (error "Label not found: " name)
            name)))) ; if non-strict, return the label name

;;
;; --- parse data ---

(define data-string-regex (regexp "^\"([^\"]*)\"(\\s+|$)"))
(define data-byte-regex (regexp "^[0-9A-Z]{2}(\\s+|$)"))

(define (data? line)
  (string-prefix? "DATA " line))

;; attempt to extract a byte value (e.g. "09" or "A3") from the beginning of
;; string <s>. if no match, return #f. otherwise, return a list (codes rest)
;; where <codes> is a single-item list of bytes found, and <rest> is the rest
;; of the line to be matched.
(define (extract-data-bytes s)
  (let ((match (string-search-positions data-byte-regex s)))
    (if match
        (let* ((pair (first match))
               (matched (substring s (first pair) (second pair)))
               (hex-string (string-append "#x" (string-trim-both matched)))
               (byte-value (string->number hex-string))
               (rest (substring s (second pair))))
          (list (list byte-value) rest))
        #f)))

(define (extract-data-string s)
  (let ((match (string-search-positions data-string-regex s)))
    (if match
        (let* ((pair (second match))
               (total-match (first match))
               (matched (substring s (first pair) (second pair)))
               (chars (string->list matched))
               (bytes (map char->integer chars)) ; fixme: PETSCII != ASCII
               (rest (substring s (second total-match))))
          (list bytes rest))
        #f)))

(define (parse-data line)
  (assert (data? line))

  (define (extract-next-data-piece s acc)
    
    (define (accumulate-and-continue result)
      (let ((bytes (first result))
            (rest (second result)))
        (extract-next-data-piece rest (append acc bytes))))

    (let ((s (string-trim s))) ; trim left
      (cond
       ((extract-data-string s) identity
        => accumulate-and-continue)

       ((extract-data-bytes s) identity
        => accumulate-and-continue)

       (else acc))))

  (define s (string-drop line 5)) ; remove leading "DATA " part
  (extract-next-data-piece s '()))

;;
;; --- opcode handling ---

(define quick-opcode-regex (regexp "^\\s*([A-Z]{3}).*"))

;; assemble a single line of assembler code.  updates state (if appropriate)
;; and returns a list of byte values (possibly empty, depending on what the
;; line of code was).
;;
(define (assemble-opcode-line state line)
  (let* ((name (get-opcode-name line))
         (opcodes (get-opcodes-with-name name))
         (result (find-matching-opcode line opcodes))
         (opcode (first result))
         (match (second result))
         (byte-value (opcode-value opcode))
         (operand (second match)))
    (cond
     ;; operand is a number (must start with $)
     ((string-prefix? "$" operand)
      (process-opcode-with-operand state byte-value operand))

     ;; operand is empty
     ((equal? "" operand)
      (list byte-value))

     ;; otherwise, operand is a label
     (else (process-opcode-with-label state byte-value operand)))))

;; return a list of bytes for the given opcode and operand.
(define (process-opcode-with-operand state byte-value operand)
  (let ((values (get-values-from-operand operand)))
    (if (branch-opcode? byte-value)
        (let* ((address (make-address (first values) (second values)))
               (ud (unsigned-delta state address)))
          (list byte-value ud))
        (append (list byte-value) values))))

;; return a list of bytes for the given opcode and label.  if the label is not
;; defined yet, insert it as a string.  (will be resolved in pass 2)
(define (process-opcode-with-label state byte-value operand)
  (let ((address (lookup-label state operand #f)))
    (if (string? address)
        (list byte-value operand)
        (if (branch-opcode? byte-value)
            (let* ((ud (unsigned-delta state address)))
              (list byte-value ud))
            (list byte-value (low address) (high address))))))

(define (unsigned-delta state address)
  (let* ((current-address (assembler-state-current-address state))
         (delta (make-relative address (+ 2 current-address))))
    (signed->unsigned delta)))

;; get a list of values from an operand string.  this list may be one or two
;; elements long.  e.g.
;; "$02" => (#x02)
;; "$1234" => (#x34 #x12)
(define (get-values-from-operand operand)
  (let* ((s (substring operand 1))
         (hexs (string-append "#x" s))
         (value (string->number hexs)))
    (if (= 2 (string-length s))
        (list value)
        (list (low value) (high value)))))

;; extract the opcode name from the given line.
;; (e.g. "LDA #$03" => "LDA")
(define (get-opcode-name line)
  (let ((match (string-search quick-opcode-regex line)))
    (if match
        (second match)
        (error "Unknown opcode in line: " line))))

;; get a list of opcodes (as 4-element lists) that match the given name.  name
;; is a string, e.g. "LDA".
(define (get-opcodes-with-name name)
  (define opcode-symbol (string->symbol (string-downcase name)))
  (define (selected? opcode)
    (equal? (opcode-name opcode) opcode-symbol))
  (filter selected? *opcodes*))

(define (find-matching-opcode line opcodes)
  (if (null? opcodes)
      (error "No matching opcode found for line: " line)
      (let* ((opcode (car opcodes))
             (regex (opcode-regex opcode))
             (match (string-search regex line)))
        (if match
            (list opcode (cdr match))
            (find-matching-opcode line (cdr opcodes))))))

;;
;; --- assembling ---

;; calculate the size of a list of bytes, allowing for unresolved labels.
(define (calc-size codes)
  (define (size x)
    (if (string? x) 2 1))
  (define sizes (map size codes))
  (reduce + 0 sizes))

;; Assembles a single line, updates the state, and returns a list of codes
;; (if appropriate).
(define (assemble-line state line)
  (cond
   ((= (string-length line) 0)
    '())

   ((label? line)
    (let ((label-name (extract-label-name line)))
      (register-label state label-name)
      '()))

   ((data? line)
    (parse-data line))

   (else (assemble-opcode-line state line))))

;; assembler pass 1: assemble all the opcodes, figure out labels, leave
;; undefined labels as strings (until the second pass).
(define (assembler-pass-1 state)

  ;; assemble the given line, update the current address, then return the
  ;; byte values
  (define (assemble-and-update line)
    (let* ((result (assemble-line state line))
           (curr-address (assembler-state-current-address state))
           (new-address (+ curr-address (calc-size result))))
      (assembler-state-current-address-set! state new-address)
      result))

  (flatten (map assemble-and-update (assembler-state-source-lines state))))

;; assembler pass 2: resolve unknown labels if possible
(define (assembler-pass-2 state codes)
  (define (pass2-aux rest acc)
    (if (null? rest)
        (reverse acc)
        (let ((x (car rest)))
          (if (string? x)
              (let ((address (lookup-label state x #t)))
                (if (branch-opcode? (car acc))
                    ;; it's a branching address
                    (let* ((start-address (assembler-state-start-address state))
                           (curr-address (+ start-address (length acc) 2))
                           (delta (make-relative address curr-address))
                           (ud (signed->unsigned delta)))
                      (pass2-aux (cdr rest) (cons ud acc)))
                    ;; it's a regular address; store as two bytes
                    (let* ((hi (high address))
                           (lo (low address)))
                      (pass2-aux (cdr rest) (cons hi (cons lo acc))))))
              (pass2-aux (cdr rest) (cons x acc))))))
  (pass2-aux codes '()))

(define (assemble state #!key start-address current-address)
  (when start-address
    (assembler-state-start-address-set! state start-address))
  (when current-address
    (assembler-state-current-address-set! state current-address))
  (let* ((codes (assembler-pass-1 state)))
    (assembler-pass-2 state codes)))

;;
;; --- write program file ---

(define (write-program state filename bytes)
  (let* ((hi (high (assembler-state-start-address state)))
         (lo (low (assembler-state-start-address state)))
         (bytes (append (list lo hi) bytes))
         (chars (map integer->char bytes)))
    (with-output-to-file filename
      (lambda ()
        (for-each write-char chars)))))

;;
;; --- toplevel stuff ---

(define (assemble-and-write filename start-address current-address lines)
  (when (string? lines)
    (define lines (string-split lines "\n")))
  (define state (make-assembler-state))
  (load-assembler-code state lines)
  (define bytes (assemble state start-address: start-address
                          current-address: current-address))
  (write-program state filename bytes)
  (printf "~a written (~a bytes)~n" filename (length bytes)))

;;
;; --- main program (invoke with csi -ss) ---

(define *usage* "
assembler.scm [options] source target

...")

(define (main args)
  (let* ((source-filename (first args))
         (target-filename (second args)))
    (with-input-from-file source-filename
      (lambda ()
        (define lines (read-lines))
        (assemble-and-write target-filename #xC000 #xC000 lines)))))
        ;; FIXME: addresses should not be hardcoded, of course

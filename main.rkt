#lang racket

(require "stage07.rkt" "stage08.rkt")

; translate-vm-to-hack is a function that takes a string vm-instructions as input
; and returns a string of Hack assembly code that represents the equivalent machine code
; of the input vm-instructions in the Hack Virtual Machine.
(define (translate-vm-to-hack vm-instructions output-port folder-name vm-file-name)
  ;; Split the vm-instructions string into a list of words using space as a separator.
  (define words (string-split vm-instructions " "))
  
  ;; Use the case statement to determine what kind of VM command is being translated.
  
    ;; If the length of words is 1, it is an arithmetic command.
    (define command (list-ref words 0))
     (case command
       (("add\r") (binary-code "+"  output-port))
       (("sub\r") (binary-code "-" output-port))
       (("neg\r") (unary-code "-" output-port))
       (("eq\r") (compare "JEQ" output-port))
       (("gt\r") (compare "JGT" output-port))
       (("lt\r") (compare "JLT" output-port))
       (("and\r") (binary-code "&" output-port))
       (("or\r") (binary-code "|" output-port))
       (("not\r") (unary-code "!" output-port))
       (("label") (define label-name (substring (list-ref words 1) 0 (sub1 (string-length (list-ref words 1))))) (label vm-file-name label-name output-port))
       (("goto") (define label-name (substring (list-ref words 1) 0 (sub1 (string-length (list-ref words 1))))) (goto vm-file-name label-name output-port))
       (("if-goto") (define label-name (substring (list-ref words 1) 0 (sub1 (string-length (list-ref words 1))))) (if-goto vm-file-name label-name output-port))
       (("push") (push (list-ref words 1) (list-ref words 2) output-port folder-name))
       (("pop") (pup (list-ref words 1) (list-ref words 2) output-port folder-name))
       (("call") (call (list-ref words 1) (list-ref words 2) output-port))
       (("function") (function (list-ref words 1) (list-ref words 2) output-port))
       (("return") (return output-port))))


; This function takes a directory, a VM file, and an output file as arguments.
(define (process-vm-file directory vm-file folder-name)
  ; It opens the input and output files and displays the name of the VM file (without the .vm extension)
  ; followed by a newline character to the output file.
  (define input-port (open-input-file (string-append directory "/" (path->string vm-file))))
  (define vm-file-name (path->string vm-file))
  (define output-port (open-output-file (string-append directory "\\" folder-name ".asm") #:exists 'append))
  
  ; It then reads lines from the input file and processes them using HandleBuy or HandleSell depending on whether
  ; the line starts with "buy" or "sell". When it reaches the end of the input file, it closes the input and output files.
  (let loop ((line (read-line input-port)))
    (cond
      ; If the end of the input file is reached, close the input and output files.
      [(eof-object? line)
       (close-input-port input-port)
       (close-output-port output-port)]
      
      ; If the line starts with "buy", use HandleBuy to process it.
      [(translate-vm-to-hack line output-port folder-name (list-ref (string-split(path->string vm-file) ".") 0))
       
       ; After processing the line, read the next line from the input file.
       (loop (read-line input-port))])))


; This function takes a directory and an output file as arguments.
(define (process-directory directory folder-name)
  ; It uses directory-list to obtain a list of VM files in the directory, and then applies process-vm-file to each file in turn.
  (define (vm-file? file)
     (regexp-match? (regexp-quote ".vm") (path->string file)))
  (let ((vm-files (filter vm-file? (directory-list directory))))
    (for-each (lambda (vm-file) (process-vm-file directory vm-file folder-name)) vm-files)))

(define (main directory)
  (define list (string-split directory "\\"))
  (define folder-name (list-ref list (- (length list) 1)))
  ; Process the directory and write the output to the output file
  (process-directory directory folder-name))
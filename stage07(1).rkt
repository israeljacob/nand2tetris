
#lang racket
(define num 0)
  
; Produces the assembly code for a unary operator
; operator: a string representing the operator to use
(define (unary-code operator)
  (string-append  "@SP\nA=M-1\nM=" operator "M\n\n"))

; Produces the assembly code for a binary operator
; operator: a string representing the operator to use
(define (binary-code operator)
  (string-append "@SP\nA=M-1\nA=A-1\nD=M\nA=A+1\nD=D" operator "M\nA=A-1\nM=D\n@SP\nM=M-1\n\n"))

; Produces the assembly code for a comparison command
; command: a string representing the comparison command to use
; num: an integer representing a unique number to use in the labels
(define (compare command num)
  (set! num (+ num 1))
  (string-append "@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@TRUE_"(number->string num)"\nD;" command "\n@SP\nA=M-1\nA=A-1\nM=0\n@END_"(number->string num)"\n0;JMP\n(TRUE_"(number->string num)  ")\n@SP\nA=M-1\nA=A-1\nM=-1\n(END_"(number->string num) ")\n@SP\nM=M-1\n\n"))

; translate-vm-to-hack is a function that takes a string vm-instructions as input
; and returns a string of Hack assembly code that represents the equivalent machine code
; of the input vm-instructions in the Hack Virtual Machine.

(define (translate-vm-to-hack vm-instructions output-port folder-name)
  ;; Split the vm-instructions string into a list of words using space as a separator.
  (define words (string-split vm-instructions " "))
  
  ;; Use the case statement to determine what kind of VM command is being translated.
  (case (length words)
    ;; If the length of words is 1, it is an arithmetic command.
    ((1)
     (case vm-instructions
       ;; The following are arithmetic commands:
       ;; add: pops the top two elements from the stack and pushes their sum.
       ;; sub: pops the top two elements from the stack and pushes their difference.
       ;; neg: negates the top element of the stack.
       ;; eq: pops the top two elements from the stack and pushes 1 if they are equal, 0 otherwise.
       ;; gt: pops the top two elements from the stack and pushes 1 if the second element is greater than the top element, 0 otherwise.
       ;; lt: pops the top two elements from the stack and pushes 1 if the second element is less than the top element, 0 otherwise.
       ;; and: pops the top two elements from the stack and pushes their bitwise and.
       ;; or: pops the top two elements from the stack and pushes their bitwise or.
       ;; not: negates the top element of the stack bitwise.
       (("add\r") (display (binary-code "+") output-port))
       (("sub\r") (display (binary-code "-") output-port))
       (("neg\r") (display (unary-code "-") output-port))
       (("eq\r")(display (compare "JEQ") output-port))
       (("gt\r") (display (compare "JGT") output-port))
       (("lt\r") (display (compare "JLT") output-port))
       (("and\r") (display (binary-code "&") output-port))
       (("or\r") (display (binary-code "|") output-port))
       (("not\r") (display (unary-code "!") output-port))))
    
    ;; If the length of words is 2, it is either a push or pop command.
    ((3)
     (define segment (list-ref words 0))
     (define command (list-ref words 1))
     (define index (list-ref words 2))
     (case segment
       ;; Push command
       (("push")
        (case command
          ;; argument: push the value of the argument segment[index] onto the stack.
          ;; local: push the value of the local segment[index] onto the stack.
          ;; static: push the value of the static segment[index] onto the stack.
          ;; constant: push the value of the constant index onto the stack.
          ;; this: push the value of the this segment[index] onto the stack.
          ;; that: push the value of the that segment[index] onto the stack.
          ;; pointer: push the value of the pointer segment[index] onto the stack.
          ;; temp: push the value of the temp segment[index] onto the stack.
          (("argument") (display (string-append "@ARG\nD=M\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("local") (display (string-append "@LCL\nD=M\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("static") (display (string-append "@" (string-append folder-name "." index) "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("constant") (display (string-append "@" index "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("this") (display (string-append "@THIS\nD=M\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("that") (display (string-append "@THAT\nD=M\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("pointer") (display (string-append "@THIS\nD=A\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))
          (("temp") (display (string-append "@5\nD=A\n@" index "\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n") output-port))))

       ;; Pop command
       (("pop")
        (case command
          ;; argument: pop the top value of the stack and store it in argument segment[index].
          ;; local: pop the top value of the stack and store it in local segment[index].
          ;; static: pop the top value of the stack and store it in static segment[index].
          ;; this: pop the top value of the stack and store it in this segment[index].
          ;; that: pop the top value of the stack and store it in that segment[index].
          ;; pointer: pop the top value of the stack and store it in pointer segment[index].
          ;; temp: pop the top value of the stack and store it in temp segment[index].
          (("argument") (display (string-append "@ARG\nD=M\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("local") (display (string-append "@LCL\nD=M\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("static") (display (string-append "@SP\nA=M-1\nD=M\n@" (string-append folder-name "." index) "\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("this") (display (string-append "@THIS\nD=M\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("that") (display (string-append "@THAT\nD=M\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("pointer") (display (string-append "@THIS\nD=A\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))
          (("temp") (display (string-append "@5\nD=A\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))))))))




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
      [(translate-vm-to-hack line output-port folder-name)
       
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

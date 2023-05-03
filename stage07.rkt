
#lang racket
(define num 0)

(provide unary-code binary-code compare push pup)

; Produces the assembly code for a unary operator
; operator: a string representing the operator to use
(define (unary-code operator output-port)
  (display (string-append "@SP\nA=M-1\nM=" operator "M\n\n") output-port))

; Produces the assembly code for a binary operator
; operator: a string representing the operator to use
(define (binary-code operator output-port)
  (display (string-append "@SP\nA=M-1\nA=A-1\nD=M\nA=A+1\nD=D" operator "M\nA=A-1\nM=D\n@SP\nM=M-1\n\n") output-port))

; Produces the assembly code for a comparison command
; command: a string representing the comparison command to use
; num: an integer representing a unique number to use in the labels
(define (compare command output-port)
  (set! num (+ num 1))
  (display (string-append "@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@TRUE_"(number->string num)"\nD;" command
                 "\n@SP\nA=M-1\nA=A-1\nM=0\n@END_"(number->string num)"\n0;JMP\n(TRUE_"(number->string num)
                 ")\n@SP\nA=M-1\nA=A-1\nM=-1\n(END_"(number->string num) ")\n@SP\nM=M-1\n\n") output-port))



(define (push segment index output-port folder-name)
  (case segment
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
  
  
  (define (pup segment index output-port folder-name)
    (case segment
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
          (("temp") (display (string-append "@5\nD=A\n@" index "\nD=D+A\n@R13\nM=D\n@SP\nA=M-1\nD=M\n@R13\nA=M\nM=D\n@SP\nM=M-1\n\n") output-port))))


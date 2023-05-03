#lang racket

(provide label goto if-goto call function return)

(define (label file-name label-name output-port)
  (display (string-append "(" file-name "." label-name ")\n\n") output-port))

(define (goto file-name label-name output-port)
  (display (string-append "@" file-name "." label-name "\n0;JMP\n\n") output-port))

(define (if-goto file-name label-name output-port)
  (display (string-append "@SP\nM=M-1\nA=M\nD=M\n@" file-name "." label-name "\nD;JNE\n\n") output-port))

(define (call label-name arg-num output-port)
  (define const-str "@SP\nA=M\nM=D\n@SP\nM=M+1\n\n")
  (display (string-append "@" label-name ".ReturnAddress\nD=A\n" const-str
                          "@LCL\nD=M\n" const-str
                          "@ARG\nD=M\n" const-str
                          "@THIS\nD=M\n" const-str
                          "@THAT\nD=M\n" const-str
                          "@SP\nD=M\n@" (number->string (- arg-num 5)) "\nD=D-A\n@ARG\nM=D\n\n"
                          "@SP\nD=M\n@LCL\nM=D\n\n"
                          "@" label-name "\n\n")
           output-port)
  (label label-name "ReturnAddres" output-port))

(define (function label-name loc-num output-port)
  (display (string-append "(" label-name ")\n\n"
                          "@" loc-num "\nD=A\n@f.End\nD;JEQ\n(f.Loop)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@f.Loop\nD=D-1;JNE\n(f.End)\n\n")
           output-port))

(define (return output-port)
  (display (string-append "@LCL\nD=M\n\n"
                          "@5\nA=D-A\nD=M\n@13\nM=D\n\n"
                          "@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nD=M\n\n"
                          "@ARG\nD=M\n@SP\nM=D+1\n\n"
                          "@LCL\nM=M-1\nA=M\nD=M\n@THAT\nM=D\n\n"
                          "@LCL\nM=M-1\nA=M\nD=M\n@THIS\nM=D\n\n"
                          "@LCL\nM=M-1\nA=M\nD=M\n@ARG\nM=D\n\n"
                          "@LCL\nM=M-1\nA=M\nD=M\n@LCL\nM=D\n\n"
                          "@13\nA=M\n0;JMP\n\n")
           output-port))
@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@0
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

(BasicLoop.LOOP_START)

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@LCL
D=M
@0	
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D-M
A=A-1
M=D
@SP
M=M-1

@ARG
D=M
@0
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
A=M
D=M
@BasicLoop.LOOP_START
D;JNE

@LCL
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=A
@1
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@0
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@1
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D-M
A=A-1
M=D
@SP
M=M-1

@ARG
D=M
@0
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

(FibonacciSeries.MAIN_LOOP_START)

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
A=M
D=M
@FibonacciSeries.COMPUTE_ELEMENT
D;JNE

@FibonacciSeries.END_PROGRAM
0;JMP

(FibonacciSeries.COMPUTE_ELEMENT)

@THAT
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@THAT
D=M
@2
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@THIS
D=A
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@THIS
D=A
@1
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D-M
A=A-1
M=D
@SP
M=M-1

@ARG
D=M
@0
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

@FibonacciSeries.MAIN_LOOP_START
0;JMP

(FibonacciSeries.END_PROGRAM)

(SimpleFunction.test)

@2
D=A
@SimpleFunction.test.End
D;JEQ
(SimpleFunction.test.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SimpleFunction.test.Loop
D=D-1;JNE
(SimpleFunction.test.End)

@LCL
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
M=!M

@ARG
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@ARG
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
A=A-1
D=M
A=A+1
D=D-M
A=A-1
M=D
@SP
M=M-1

@LCL
D=M

@5
A=D-A
D=M
@13
M=D

@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D

@ARG
D=M
@SP
M=D+1

@LCL
M=M-1
A=M
D=M
@THAT
M=D

@LCL
M=M-1
A=M
D=M
@THIS
M=D

@LCL
M=M-1
A=M
D=M
@ARG
M=D

@LCL
M=M-1
A=M
D=M
@LCL
M=D

@13
A=M
0;JMP


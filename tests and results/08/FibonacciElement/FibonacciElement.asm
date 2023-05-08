@256
D=A
@SP
M=D

@Sys.init.ReturnAddress1
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@0
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Sys.init
0;JMP

(Sys.init.ReturnAddress1)

(Main.fibonacci)

@0
D=A
@Main.fibonacci.End
D;JEQ
(Main.fibonacci.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Main.fibonacci.Loop
D=D-1;JNE
(Main.fibonacci.End)

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
D=M
A=A-1
D=M-D
@TRUE_1
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_1
0;JMP
(TRUE_1)
@SP
A=M-1
A=A-1
M=-1
(END_1)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Main.IF_TRUE
D;JNE

@Main.IF_FALSE
0;JMP

(Main.IF_TRUE)

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

(Main.IF_FALSE)

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

@Main.fibonacci.ReturnAddress3
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@1
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Main.fibonacci
0;JMP

(Main.fibonacci.ReturnAddress3)

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

@Main.fibonacci.ReturnAddress4
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@1
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Main.fibonacci
0;JMP

(Main.fibonacci.ReturnAddress4)

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

(Sys.init)

@0
D=A
@Sys.init.End
D;JEQ
(Sys.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.init.Loop
D=D-1;JNE
(Sys.init.End)

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

@Main.fibonacci.ReturnAddress6
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@1
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Main.fibonacci
0;JMP

(Main.fibonacci.ReturnAddress6)

(Sys.WHILE)

@Sys.WHILE
0;JMP


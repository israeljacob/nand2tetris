

(SimpleFunction.test)

@2
D=A
@f.End
D;JEQ
(f.Loop)
@SP
A=M
M=0
@SP
M=M+1
@f.Loop
D=D-1;JNE
(f.End)

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


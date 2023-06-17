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

@4000	//
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=A
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

@5000
D=A
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

@Sys.main.ReturnAddress3
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

@Sys.main
0;JMP

(Sys.main.ReturnAddress3)

@5
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

(Sys.LOOP)

@Sys.LOOP
0;JMP

(Sys.main)

@5
D=A
@Sys.main.End
D;JEQ
(Sys.main.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.main.Loop
D=D-1;JNE
(Sys.main.End)

@4001
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=A
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

@5001
D=A
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

@200
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
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

@40
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@3
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

@123
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.add12.ReturnAddress5
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

@Sys.add12
0;JMP

(Sys.add12.ReturnAddress5)

@5
D=A
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

@LCL
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@4
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

(Sys.add12)

@0
D=A
@Sys.add12.End
D;JEQ
(Sys.add12.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.add12.Loop
D=D-1;JNE
(Sys.add12.End)

@4002
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=A
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

@5002
D=A
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

@12
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


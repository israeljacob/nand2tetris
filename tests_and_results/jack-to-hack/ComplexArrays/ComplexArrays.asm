(Array.new)

@0
D=A
@Array.new.End
D;JEQ
(Array.new.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Array.new.Loop
D=D-1;JNE
(Array.new.End)

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

@0
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
D;JGT
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
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Array.IF_TRUE0
D;JNE

@Array.IF_FALSE0
0;JMP

(Array.IF_TRUE0)

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress2
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress2)

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

(Array.IF_FALSE0)

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

@Memory.alloc.ReturnAddress3
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

@Memory.alloc
0;JMP

(Memory.alloc.ReturnAddress3)

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

(Array.dispose)

@0
D=A
@Array.dispose.End
D;JEQ
(Array.dispose.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Array.dispose.Loop
D=D-1;JNE
(Array.dispose.End)

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

@THIS
D=A
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Memory.deAlloc.ReturnAddress5
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

@Memory.deAlloc
0;JMP

(Memory.deAlloc.ReturnAddress5)

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

@0
D=A
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

(Keyboard.init)

@0
D=A
@Keyboard.init.End
D;JEQ
(Keyboard.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Keyboard.init.Loop
D=D-1;JNE
(Keyboard.init.End)

@0
D=A
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

(Keyboard.keyPressed)

@0
D=A
@Keyboard.keyPressed.End
D;JEQ
(Keyboard.keyPressed.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Keyboard.keyPressed.Loop
D=D-1;JNE
(Keyboard.keyPressed.End)

@24576
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.peek.ReturnAddress8
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

@Memory.peek
0;JMP

(Memory.peek.ReturnAddress8)

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

(Keyboard.readChar)

@2
D=A
@Keyboard.readChar.End
D;JEQ
(Keyboard.readChar.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Keyboard.readChar.Loop
D=D-1;JNE
(Keyboard.readChar.End)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.printChar.ReturnAddress10
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress10)

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

(Keyboard.WHILE_EXP0)

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

@0
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
@TRUE_2
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_2
0;JMP
(TRUE_2)
@SP
A=M-1
A=A-1
M=-1
(END_2)
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

@0
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
@TRUE_3
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_3
0;JMP
(TRUE_3)
@SP
A=M-1
A=A-1
M=-1
(END_3)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Keyboard.WHILE_END0
D;JNE

@Keyboard.keyPressed.ReturnAddress11
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

@Keyboard.keyPressed
0;JMP

(Keyboard.keyPressed.ReturnAddress11)

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

@0
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
@TRUE_4
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_4
0;JMP
(TRUE_4)
@SP
A=M-1
A=A-1
M=-1
(END_4)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Keyboard.IF_TRUE0
D;JNE

@Keyboard.IF_FALSE0
0;JMP

(Keyboard.IF_TRUE0)

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

(Keyboard.IF_FALSE0)

@Keyboard.WHILE_EXP0
0;JMP

(Keyboard.WHILE_END0)

@String.backSpace.ReturnAddress12
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

@String.backSpace
0;JMP

(String.backSpace.ReturnAddress12)

@Output.printChar.ReturnAddress13
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress13)

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Output.printChar.ReturnAddress14
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress14)

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

(Keyboard.readLine)

@5
D=A
@Keyboard.readLine.End
D;JEQ
(Keyboard.readLine.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Keyboard.readLine.Loop
D=D-1;JNE
(Keyboard.readLine.End)

@80
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress16
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

@String.new
0;JMP

(String.new.ReturnAddress16)

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

@Output.printString.ReturnAddress17
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress17)

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

@String.newLine.ReturnAddress18
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

@String.newLine
0;JMP

(String.newLine.ReturnAddress18)

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

@String.backSpace.ReturnAddress19
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

@String.backSpace
0;JMP

(String.backSpace.ReturnAddress19)

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

(Keyboard.WHILE_EXP0)

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
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Keyboard.WHILE_END0
D;JNE

@Keyboard.readChar.ReturnAddress20
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

@Keyboard.readChar
0;JMP

(Keyboard.readChar.ReturnAddress20)

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
D=M
A=A-1
D=M-D
@TRUE_5
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_5
0;JMP
(TRUE_5)
@SP
A=M-1
A=A-1
M=-1
(END_5)
@SP
M=M-1

@LCL
D=M
@4
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
M=!M

@SP
M=M-1
A=M
D=M
@Keyboard.IF_TRUE0
D;JNE

@Keyboard.IF_FALSE0
0;JMP

(Keyboard.IF_TRUE0)

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
@2
A=D+A
D=M
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
@TRUE_6
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_6
0;JMP
(TRUE_6)
@SP
A=M-1
A=A-1
M=-1
(END_6)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Keyboard.IF_TRUE1
D;JNE

@Keyboard.IF_FALSE1
0;JMP

(Keyboard.IF_TRUE1)

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

@String.eraseLastChar.ReturnAddress21
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

@String.eraseLastChar
0;JMP

(String.eraseLastChar.ReturnAddress21)

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

@Keyboard.IF_END1
0;JMP

(Keyboard.IF_FALSE1)

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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress22
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress22)

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

(Keyboard.IF_END1)

(Keyboard.IF_FALSE0)

@Keyboard.WHILE_EXP0
0;JMP

(Keyboard.WHILE_END0)

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

(Keyboard.readInt)

@2
D=A
@Keyboard.readInt.End
D;JEQ
(Keyboard.readInt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Keyboard.readInt.Loop
D=D-1;JNE
(Keyboard.readInt.End)

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

@Keyboard.readLine.ReturnAddress24
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

@Keyboard.readLine
0;JMP

(Keyboard.readLine.ReturnAddress24)

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

@String.intValue.ReturnAddress25
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

@String.intValue
0;JMP

(String.intValue.ReturnAddress25)

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

@String.dispose.ReturnAddress26
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

@String.dispose
0;JMP

(String.dispose.ReturnAddress26)

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

(Main.main)

@3
D=A
@Main.main.End
D;JEQ
(Main.main.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Main.main.Loop
D=D-1;JNE
(Main.main.End)

@10
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress28
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

@Array.new
0;JMP

(Array.new.ReturnAddress28)

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

@5
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress29
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

@Array.new
0;JMP

(Array.new.ReturnAddress29)

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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress30
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

@Array.new
0;JMP

(Array.new.ReturnAddress30)

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

@3
D=A
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

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@4
D=A
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

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@5
D=A
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

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@3
D=A
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

@3
D=A
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

@3
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

@5
D=A
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

@3
D=A
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

@5
D=A
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

@7
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
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

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@Main.double.ReturnAddress31
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

@Main.double
0;JMP

(Main.double.ReturnAddress31)

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

@Math.multiply.ReturnAddress32
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress32)

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

@5
D=A
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

@0
D=A
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@0
D=A
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

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress33
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

@String.new
0;JMP

(String.new.ReturnAddress33)

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress34
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress34)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress35
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress35)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress36
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress36)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress37
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress37)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress38
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress38)

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress39
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress39)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress40
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress40)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress41
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress41)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress42
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress42)

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress43
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress43)

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress44
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress44)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress45
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress45)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress46
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress46)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress47
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress47)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress48
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress48)

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress49
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress49)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress50
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress50)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress51
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress51)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress52
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress52)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress53
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress53)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress54
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress54)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress55
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress55)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress56
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress56)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress57
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress57)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress58
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress58)

@53
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress59
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress59)

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress60
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress60)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress61
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress61)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress62
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress62)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress63
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress63)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress64
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress64)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress65
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress65)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress66
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress66)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress67
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress67)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress68
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress68)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress69
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress69)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress70
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress70)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress71
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress71)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress72
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress72)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress73
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress73)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress74
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress74)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress75
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress75)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress76
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress76)

@Output.printString.ReturnAddress77
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress77)

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

@2
D=A
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

@Output.printInt.ReturnAddress78
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress78)

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

@Output.println.ReturnAddress79
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

@Output.println
0;JMP

(Output.println.ReturnAddress79)

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

@44
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress80
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

@String.new
0;JMP

(String.new.ReturnAddress80)

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress81
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress81)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress82
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress82)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress83
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress83)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress84
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress84)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress85
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress85)

@50
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress86
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress86)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress87
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress87)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress88
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress88)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress89
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress89)

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress90
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress90)

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress91
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress91)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress92
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress92)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress93
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress93)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress94
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress94)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress95
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress95)

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress96
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress96)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress97
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress97)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress98
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress98)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress99
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress99)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress100
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress100)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress101
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress101)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress102
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress102)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress103
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress103)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress104
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress104)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress105
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress105)

@52
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress106
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress106)

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress107
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress107)

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress108
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress108)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress109
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress109)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress110
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress110)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress111
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress111)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress112
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress112)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress113
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress113)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress114
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress114)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress115
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress115)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress116
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress116)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress117
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress117)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress118
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress118)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress119
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress119)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress120
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress120)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress121
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress121)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress122
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress122)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress123
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress123)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress124
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress124)

@Output.printString.ReturnAddress125
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress125)

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

@5
D=A
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

@Output.printInt.ReturnAddress126
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress126)

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

@Output.println.ReturnAddress127
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

@Output.println
0;JMP

(Output.println.ReturnAddress127)

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

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress128
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

@String.new
0;JMP

(String.new.ReturnAddress128)

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress129
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress129)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress130
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress130)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress131
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress131)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress132
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress132)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress133
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress133)

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress134
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress134)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress135
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress135)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress136
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress136)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress137
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress137)

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress138
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress138)

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress139
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress139)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress140
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress140)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress141
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress141)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress142
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress142)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress143
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress143)

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress144
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress144)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress145
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress145)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress146
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress146)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress147
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress147)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress148
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress148)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress149
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress149)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress150
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress150)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress151
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress151)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress152
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress152)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress153
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress153)

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress154
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress154)

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress155
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress155)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress156
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress156)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress157
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress157)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress158
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress158)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress159
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress159)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress160
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress160)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress161
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress161)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress162
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress162)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress163
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress163)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress164
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress164)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress165
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress165)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress166
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress166)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress167
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress167)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress168
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress168)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress169
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress169)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress170
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress170)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress171
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress171)

@Output.printString.ReturnAddress172
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress172)

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
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Output.printInt.ReturnAddress173
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress173)

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

@Output.println.ReturnAddress174
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

@Output.println
0;JMP

(Output.println.ReturnAddress174)

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

@0
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

@0
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
@TRUE_7
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_7
0;JMP
(TRUE_7)
@SP
A=M-1
A=A-1
M=-1
(END_7)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Main.IF_TRUE0
D;JNE

@Main.IF_FALSE0
0;JMP

(Main.IF_TRUE0)

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

@10
D=A
@SP
A=M
M=D
@SP
M=M+1

@Main.fill.ReturnAddress175
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Main.fill
0;JMP

(Main.fill.ReturnAddress175)

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

@3
D=A
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

@1
D=A
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

@33
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@7
D=A
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

@1
D=A
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

@77
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@3
D=A
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

@1
D=A
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

@1
D=A
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

@1
D=A
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

@5
D=A
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

(Main.IF_FALSE0)

@44
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress176
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

@String.new
0;JMP

(String.new.ReturnAddress176)

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress177
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress177)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress178
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress178)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress179
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress179)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress180
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress180)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress181
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress181)

@52
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress182
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress182)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress183
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress183)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress184
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress184)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress185
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress185)

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress186
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress186)

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress187
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress187)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress188
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress188)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress189
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress189)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress190
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress190)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress191
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress191)

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress192
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress192)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress193
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress193)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress194
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress194)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress195
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress195)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress196
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress196)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress197
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress197)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress198
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress198)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress199
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress199)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress200
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress200)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress201
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress201)

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress202
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress202)

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress203
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress203)

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress204
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress204)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress205
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress205)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress206
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress206)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress207
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress207)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress208
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress208)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress209
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress209)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress210
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress210)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress211
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress211)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress212
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress212)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress213
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress213)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress214
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress214)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress215
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress215)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress216
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress216)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress217
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress217)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress218
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress218)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress219
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress219)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress220
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress220)

@Output.printString.ReturnAddress221
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress221)

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

@1
D=A
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

@Output.printInt.ReturnAddress222
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress222)

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

@Output.println.ReturnAddress223
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

@Output.println
0;JMP

(Output.println.ReturnAddress223)

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

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.new.ReturnAddress224
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

@String.new
0;JMP

(String.new.ReturnAddress224)

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress225
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress225)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress226
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress226)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress227
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress227)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress228
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress228)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress229
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress229)

@53
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress230
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress230)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress231
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress231)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress232
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress232)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress233
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress233)

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress234
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress234)

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress235
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress235)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress236
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress236)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress237
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress237)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress238
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress238)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress239
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress239)

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress240
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress240)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress241
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress241)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress242
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress242)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress243
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress243)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress244
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress244)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress245
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress245)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress246
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress246)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress247
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress247)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress248
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress248)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress249
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress249)

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress250
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress250)

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress251
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress251)

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress252
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress252)

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress253
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress253)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress254
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress254)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress255
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress255)

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress256
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress256)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress257
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress257)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress258
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress258)

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress259
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress259)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress260
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress260)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress261
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress261)

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress262
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress262)

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress263
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress263)

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress264
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress264)

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress265
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress265)

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress266
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress266)

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress267
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress267)

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress268
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress268)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@String.appendChar.ReturnAddress269
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.appendChar
0;JMP

(String.appendChar.ReturnAddress269)

@Output.printString.ReturnAddress270
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress270)

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

@1
D=A
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

@Output.printInt.ReturnAddress271
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress271)

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

@Output.println.ReturnAddress272
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

@Output.println
0;JMP

(Output.println.ReturnAddress272)

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

@0
D=A
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

(Main.double)

@0
D=A
@Main.double.End
D;JEQ
(Main.double.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Main.double.Loop
D=D-1;JNE
(Main.double.End)

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

@Math.multiply.ReturnAddress274
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress274)

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

(Main.fill)

@0
D=A
@Main.fill.End
D;JEQ
(Main.fill.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Main.fill.Loop
D=D-1;JNE
(Main.fill.End)

(Main.WHILE_EXP0)

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

@0
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
@TRUE_8
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_8
0;JMP
(TRUE_8)
@SP
A=M-1
A=A-1
M=-1
(END_8)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Main.WHILE_END0
D;JNE

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress276
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

@Array.new
0;JMP

(Array.new.ReturnAddress276)

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

@5
D=A
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

@Main.WHILE_EXP0
0;JMP

(Main.WHILE_END0)

@0
D=A
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

(Math.init)

@1
D=A
@Math.init.End
D;JEQ
(Math.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.init.Loop
D=D-1;JNE
(Math.init.End)

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress278
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

@Array.new
0;JMP

(Array.new.ReturnAddress278)

@SP
A=M-1
D=M
@Math.1
M=D
@SP
M=M-1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress279
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

@Array.new
0;JMP

(Array.new.ReturnAddress279)

@SP
A=M-1
D=M
@Math.0
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

@Math.0
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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

(Math.WHILE_EXP0)

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

@15
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
@TRUE_9
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_9
0;JMP
(TRUE_9)
@SP
A=M-1
A=A-1
M=-1
(END_9)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.WHILE_END0
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

@Math.0
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

@Math.0
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

@Math.0
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

@5
D=A
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

@Math.WHILE_EXP0
0;JMP

(Math.WHILE_END0)

@0
D=A
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

(Math.abs)

@0
D=A
@Math.abs.End
D;JEQ
(Math.abs.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.abs.Loop
D=D-1;JNE
(Math.abs.End)

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

@0
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
@TRUE_10
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_10
0;JMP
(TRUE_10)
@SP
A=M-1
A=A-1
M=-1
(END_10)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

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
M=-M

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

(Math.IF_FALSE0)

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

(Math.multiply)

@5
D=A
@Math.multiply.End
D;JEQ
(Math.multiply.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.multiply.Loop
D=D-1;JNE
(Math.multiply.End)

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

@0
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
@TRUE_11
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_11
0;JMP
(TRUE_11)
@SP
A=M-1
A=A-1
M=-1
(END_11)
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

@0
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
@TRUE_12
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_12
0;JMP
(TRUE_12)
@SP
A=M-1
A=A-1
M=-1
(END_12)
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

@0
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
@TRUE_13
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_13
0;JMP
(TRUE_13)
@SP
A=M-1
A=A-1
M=-1
(END_13)
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

@0
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
@TRUE_14
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_14
0;JMP
(TRUE_14)
@SP
A=M-1
A=A-1
M=-1
(END_14)
@SP
M=M-1

@LCL
D=M
@4
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

@Math.abs.ReturnAddress282
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress282)

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.abs.ReturnAddress283
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress283)

@ARG
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
D=M
A=A-1
D=M-D
@TRUE_15
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_15
0;JMP
(TRUE_15)
@SP
A=M-1
A=A-1
M=-1
(END_15)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@ARG
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

(Math.IF_FALSE0)

(Math.WHILE_EXP0)

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
D=D-M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_16
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_16
0;JMP
(TRUE_16)
@SP
A=M-1
A=A-1
M=-1
(END_16)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.WHILE_END0
D;JNE

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

@Math.0
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

@0
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
@TRUE_17
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_17
0;JMP
(TRUE_17)
@SP
A=M-1
A=A-1
M=-1
(END_17)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE1
D;JNE

@Math.IF_FALSE1
0;JMP

(Math.IF_TRUE1)

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

@Math.0
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

(Math.IF_FALSE1)

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
@3
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

@Math.WHILE_EXP0
0;JMP

(Math.WHILE_END0)

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
M=M-1
A=M
D=M
@Math.IF_TRUE2
D;JNE

@Math.IF_FALSE2
0;JMP

(Math.IF_TRUE2)

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
M=-M

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

(Math.IF_FALSE2)

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

(Math.divide)

@4
D=A
@Math.divide.End
D;JEQ
(Math.divide.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.divide.Loop
D=D-1;JNE
(Math.divide.End)

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

@0
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
@TRUE_18
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_18
0;JMP
(TRUE_18)
@SP
A=M-1
A=A-1
M=-1
(END_18)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress285
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress285)

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

(Math.IF_FALSE0)

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

@0
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
@TRUE_19
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_19
0;JMP
(TRUE_19)
@SP
A=M-1
A=A-1
M=-1
(END_19)
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

@0
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
@TRUE_20
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_20
0;JMP
(TRUE_20)
@SP
A=M-1
A=A-1
M=-1
(END_20)
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

@0
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
@TRUE_21
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_21
0;JMP
(TRUE_21)
@SP
A=M-1
A=A-1
M=-1
(END_21)
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

@0
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
@TRUE_22
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_22
0;JMP
(TRUE_22)
@SP
A=M-1
A=A-1
M=-1
(END_22)
@SP
M=M-1

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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.1
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

@Math.abs.ReturnAddress286
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress286)

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

@5
D=A
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

@Math.abs.ReturnAddress287
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress287)

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

(Math.WHILE_EXP0)

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

@15
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
@TRUE_23
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_23
0;JMP
(TRUE_23)
@SP
A=M-1
A=A-1
M=-1
(END_23)
@SP
M=M-1

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

@SP
A=M-1
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.WHILE_END0
D;JNE

@32767
D=A
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

@Math.1
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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.1
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

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_24
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_24
0;JMP
(TRUE_24)
@SP
A=M-1
A=A-1
M=-1
(END_24)
@SP
M=M-1

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

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE1
D;JNE

@Math.IF_FALSE1
0;JMP

(Math.IF_TRUE1)

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

@Math.1
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
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.1
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

@Math.1
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

@5
D=A
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

@Math.1
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

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_25
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_25
0;JMP
(TRUE_25)
@SP
A=M-1
A=A-1
M=-1
(END_25)
@SP
M=M-1

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

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE2
D;JNE

@Math.IF_FALSE2
0;JMP

(Math.IF_TRUE2)

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

(Math.IF_FALSE2)

(Math.IF_FALSE1)

@Math.WHILE_EXP0
0;JMP

(Math.WHILE_END0)

(Math.WHILE_EXP1)

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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=-M

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_26
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_26
0;JMP
(TRUE_26)
@SP
A=M-1
A=A-1
M=-1
(END_26)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.WHILE_END1
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

@Math.1
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

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_27
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_27
0;JMP
(TRUE_27)
@SP
A=M-1
A=A-1
M=-1
(END_27)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE3
D;JNE

@Math.IF_FALSE3
0;JMP

(Math.IF_TRUE3)

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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.0
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

@Math.1
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

(Math.IF_FALSE3)

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

@Math.WHILE_EXP1
0;JMP

(Math.WHILE_END1)

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

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE4
D;JNE

@Math.IF_FALSE4
0;JMP

(Math.IF_TRUE4)

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
M=-M

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

(Math.IF_FALSE4)

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

(Math.sqrt)

@4
D=A
@Math.sqrt.End
D;JEQ
(Math.sqrt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.sqrt.Loop
D=D-1;JNE
(Math.sqrt.End)

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

@0
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
@TRUE_28
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_28
0;JMP
(TRUE_28)
@SP
A=M-1
A=A-1
M=-1
(END_28)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress289
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress289)

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

(Math.IF_FALSE0)

@7
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

(Math.WHILE_EXP0)

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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=-M

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_29
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_29
0;JMP
(TRUE_29)
@SP
A=M-1
A=A-1
M=-1
(END_29)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.WHILE_END0
D;JNE

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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.0
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress290
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress290)

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
D=M
A=A-1
D=M-D
@TRUE_30
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_30
0;JMP
(TRUE_30)
@SP
A=M-1
A=A-1
M=-1
(END_30)
@SP
M=M-1

@SP
A=M-1
M=!M

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

@0
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
@TRUE_31
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_31
0;JMP
(TRUE_31)
@SP
A=M-1
A=A-1
M=-1
(END_31)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE1
D;JNE

@Math.IF_FALSE1
0;JMP

(Math.IF_TRUE1)

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

(Math.IF_FALSE1)

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

@Math.WHILE_EXP0
0;JMP

(Math.WHILE_END0)

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

(Math.max)

@0
D=A
@Math.max.End
D;JEQ
(Math.max.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.max.Loop
D=D-1;JNE
(Math.max.End)

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
D=M
A=A-1
D=M-D
@TRUE_32
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_32
0;JMP
(TRUE_32)
@SP
A=M-1
A=A-1
M=-1
(END_32)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

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

@ARG
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

(Math.IF_FALSE0)

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

(Math.min)

@0
D=A
@Math.min.End
D;JEQ
(Math.min.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Math.min.Loop
D=D-1;JNE
(Math.min.End)

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
D=M
A=A-1
D=M-D
@TRUE_33
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_33
0;JMP
(TRUE_33)
@SP
A=M-1
A=A-1
M=-1
(END_33)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Math.IF_TRUE0
D;JNE

@Math.IF_FALSE0
0;JMP

(Math.IF_TRUE0)

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

@ARG
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

(Math.IF_FALSE0)

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

(Memory.init)

@0
D=A
@Memory.init.End
D;JEQ
(Memory.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Memory.init.Loop
D=D-1;JNE
(Memory.init.End)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Memory.0
M=D
@SP
M=M-1

@2048
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.0
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

@14334
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@2049
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.0
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

@2050
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@0
D=A
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

(Memory.peek)

@0
D=A
@Memory.peek.End
D;JEQ
(Memory.peek.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Memory.peek.Loop
D=D-1;JNE
(Memory.peek.End)

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

@Memory.0
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

(Memory.poke)

@0
D=A
@Memory.poke.End
D;JEQ
(Memory.poke.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Memory.poke.Loop
D=D-1;JNE
(Memory.poke.End)

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

@Memory.0
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

@5
D=A
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

@0
D=A
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

(Memory.alloc)

@2
D=A
@Memory.alloc.End
D;JEQ
(Memory.alloc.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Memory.alloc.Loop
D=D-1;JNE
(Memory.alloc.End)

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

@0
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
@TRUE_34
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_34
0;JMP
(TRUE_34)
@SP
A=M-1
A=A-1
M=-1
(END_34)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE0
D;JNE

@Memory.IF_FALSE0
0;JMP

(Memory.IF_TRUE0)

@5
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress297
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress297)

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

(Memory.IF_FALSE0)

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

@0
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
@TRUE_35
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_35
0;JMP
(TRUE_35)
@SP
A=M-1
A=A-1
M=-1
(END_35)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE1
D;JNE

@Memory.IF_FALSE1
0;JMP

(Memory.IF_TRUE1)

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.IF_FALSE1)

@2048
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

(Memory.WHILE_EXP0)

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

@16383
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
@TRUE_36
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_36
0;JMP
(TRUE_36)
@SP
A=M-1
A=A-1
M=-1
(END_36)
@SP
M=M-1

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
D=M
A=A-1
D=M-D
@TRUE_37
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_37
0;JMP
(TRUE_37)
@SP
A=M-1
A=A-1
M=-1
(END_37)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Memory.WHILE_END0
D;JNE

@1
D=A
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

@0
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
@TRUE_38
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_38
0;JMP
(TRUE_38)
@SP
A=M-1
A=A-1
M=-1
(END_38)
@SP
M=M-1

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

@16382
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
@TRUE_39
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_39
0;JMP
(TRUE_39)
@SP
A=M-1
A=A-1
M=-1
(END_39)
@SP
M=M-1

@0
D=A
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

@0
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
@TRUE_40
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_40
0;JMP
(TRUE_40)
@SP
A=M-1
A=A-1
M=-1
(END_40)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE2
D;JNE

@Memory.IF_FALSE2
0;JMP

(Memory.IF_TRUE2)

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

@Memory.IF_END2
0;JMP

(Memory.IF_FALSE2)

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

@1
D=A
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
D=D-M
A=A-1
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

@5
D=A
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
D=D+M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_41
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_41
0;JMP
(TRUE_41)
@SP
A=M-1
A=A-1
M=-1
(END_41)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE3
D;JNE

@Memory.IF_FALSE3
0;JMP

(Memory.IF_TRUE3)

@1
D=A
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
D=D+M
A=A-1
M=D
@SP
M=M-1

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

@5
D=A
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

@Memory.IF_END3
0;JMP

(Memory.IF_FALSE3)

@1
D=A
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

@1
D=A
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

@5
D=A
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

(Memory.IF_END3)

(Memory.IF_END2)

@Memory.WHILE_EXP0
0;JMP

(Memory.WHILE_END0)

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

@16379
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
@TRUE_42
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_42
0;JMP
(TRUE_42)
@SP
A=M-1
A=A-1
M=-1
(END_42)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE4
D;JNE

@Memory.IF_FALSE4
0;JMP

(Memory.IF_TRUE4)

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress298
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress298)

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

(Memory.IF_FALSE4)

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
D=D+M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_43
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_43
0;JMP
(TRUE_43)
@SP
A=M-1
A=A-1
M=-1
(END_43)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE5
D;JNE

@Memory.IF_FALSE5
0;JMP

(Memory.IF_TRUE5)

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
D=D+M
A=A-1
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
D=D-M
A=A-1
M=D
@SP
M=M-1

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

@5
D=A
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
D=D+M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_44
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_44
0;JMP
(TRUE_44)
@SP
A=M-1
A=A-1
M=-1
(END_44)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE6
D;JNE

@Memory.IF_FALSE6
0;JMP

(Memory.IF_TRUE6)

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

@3
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
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@4
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

@5
D=A
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

@Memory.IF_END6
0;JMP

(Memory.IF_FALSE6)

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

@3
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

@1
D=A
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

@5
D=A
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

(Memory.IF_END6)

@1
D=A
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
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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
D=D+M
A=A-1
M=D
@SP
M=M-1

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

@5
D=A
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

(Memory.IF_FALSE5)

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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

(Memory.deAlloc)

@2
D=A
@Memory.deAlloc.End
D;JEQ
(Memory.deAlloc.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Memory.deAlloc.Loop
D=D-1;JNE
(Memory.deAlloc.End)

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

@1
D=A
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

@0
D=A
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

@0
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
@TRUE_45
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_45
0;JMP
(TRUE_45)
@SP
A=M-1
A=A-1
M=-1
(END_45)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE0
D;JNE

@Memory.IF_FALSE0
0;JMP

(Memory.IF_TRUE0)

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

@1
D=A
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
D=D-M
A=A-1
M=D
@SP
M=M-1

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

@5
D=A
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

@Memory.IF_END0
0;JMP

(Memory.IF_FALSE0)

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

@1
D=A
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
D=D-M
A=A-1
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

@5
D=A
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
D=D+M
A=A-1
M=D
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_46
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_46
0;JMP
(TRUE_46)
@SP
A=M-1
A=A-1
M=-1
(END_46)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Memory.IF_TRUE1
D;JNE

@Memory.IF_FALSE1
0;JMP

(Memory.IF_TRUE1)

@1
D=A
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
D=D+M
A=A-1
M=D
@SP
M=M-1

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

@5
D=A
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

@Memory.IF_END1
0;JMP

(Memory.IF_FALSE1)

@1
D=A
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

@1
D=A
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

@5
D=A
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

(Memory.IF_END1)

(Memory.IF_END0)

@0
D=A
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

(Output.init)

@0
D=A
@Output.init.End
D;JEQ
(Output.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.init.Loop
D=D-1;JNE
(Output.init.End)

@16384
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.4
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

@SP
A=M-1
M=!M

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.1
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

@SP
A=M-1
D=M
@Output.0
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

@String.new.ReturnAddress301
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

@String.new
0;JMP

(String.new.ReturnAddress301)

@SP
A=M-1
D=M
@Output.3
M=D
@SP
M=M-1

@Output.initMap.ReturnAddress302
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

@Output.initMap
0;JMP

(Output.initMap.ReturnAddress302)

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

@Output.createShiftedMap.ReturnAddress303
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

@Output.createShiftedMap
0;JMP

(Output.createShiftedMap.ReturnAddress303)

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

@0
D=A
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

(Output.initMap)

@0
D=A
@Output.initMap.End
D;JEQ
(Output.initMap.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.initMap.Loop
D=D-1;JNE
(Output.initMap.End)

@127
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress305
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

@Array.new
0;JMP

(Array.new.ReturnAddress305)

@SP
A=M-1
D=M
@Output.5
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

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress306
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress306)

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

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress307
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress307)

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

@33
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress308
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress308)

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

@34
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@20
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress309
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress309)

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

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress310
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress310)

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

@36
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress311
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress311)

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

@37
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress312
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress312)

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

@38
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress313
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress313)

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

@39
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress314
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress314)

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

@40
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress315
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress315)

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

@41
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress316
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress316)

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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress317
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress317)

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

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress318
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress318)

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

@44
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress319
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress319)

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

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress320
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress320)

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

@46
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress321
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress321)

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

@47
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress322
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress322)

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

@48
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress323
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress323)

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

@49
D=A
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

@14
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
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

@12
D=A
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

@12
D=A
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

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress324
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress324)

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

@50
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress325
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress325)

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

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress326
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress326)

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

@52
D=A
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@26
D=A
@SP
A=M
M=D
@SP
M=M+1

@25
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@60
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress327
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress327)

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

@53
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress328
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress328)

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

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress329
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress329)

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

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@12
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress330
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress330)

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

@56
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress331
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress331)

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

@57
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@62
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@14
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress332
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress332)

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

@58
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress333
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress333)

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

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress334
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress334)

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

@60
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress335
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress335)

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

@61
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress336
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress336)

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

@62
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress337
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress337)

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

@64
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress338
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress338)

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

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress339
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress339)

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

@65
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress340
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress340)

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

@66
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress341
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress341)

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

@67
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress342
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress342)

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

@68
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress343
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress343)

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

@69
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress344
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress344)

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

@70
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress345
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress345)

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

@71
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@44
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress346
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress346)

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

@72
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress347
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress347)

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

@73
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
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

@12
D=A
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

@12
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress348
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress348)

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

@74
D=A
@SP
A=M
M=D
@SP
M=M+1

@60
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@14
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress349
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress349)

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

@75
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress350
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress350)

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

@76
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress351
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress351)

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

@77
D=A
@SP
A=M
M=D
@SP
M=M+1

@33
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress352
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress352)

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

@78
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress353
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress353)

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

@79
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress354
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress354)

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

@80
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress355
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress355)

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

@81
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@59
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress356
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress356)

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

@82
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress357
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress357)

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

@83
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress358
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress358)

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

@84
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
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

@12
D=A
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

@12
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress359
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress359)

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

@85
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress360
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress360)

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

@86
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress361
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress361)

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

@87
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress362
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress362)

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

@88
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress363
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress363)

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

@89
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress364
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress364)

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

@90
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@49
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@35
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress365
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress365)

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

@91
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress366
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress366)

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

@92
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
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

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress367
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress367)

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

@93
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress368
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress368)

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

@94
D=A
@SP
A=M
M=D
@SP
M=M+1

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress369
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress369)

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

@95
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress370
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress370)

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

@96
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
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

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress371
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress371)

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

@97
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@14
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress372
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress372)

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

@98
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress373
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress373)

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

@99
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress374
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress374)

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

@100
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@60
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress375
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress375)

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

@101
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress376
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress376)

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

@102
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@38
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress377
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress377)

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

@103
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@62
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress378
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress378)

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

@104
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress379
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress379)

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

@105
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@14
D=A
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

@12
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress380
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress380)

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

@106
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@56
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress381
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress381)

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

@107
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress382
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress382)

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

@108
D=A
@SP
A=M
M=D
@SP
M=M+1

@14
D=A
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

@12
D=A
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

@12
D=A
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

@12
D=A
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

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress383
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress383)

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

@109
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@29
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@43
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress384
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress384)

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

@110
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@29
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress385
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress385)

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

@111
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress386
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress386)

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

@112
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress387
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress387)

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

@113
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@62
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress388
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress388)

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

@114
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@29
D=A
@SP
A=M
M=D
@SP
M=M+1

@55
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@7
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress389
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress389)

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

@115
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress390
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress390)

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

@116
D=A
@SP
A=M
M=D
@SP
M=M+1

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@28
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress391
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress391)

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

@117
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
@SP
A=M
M=D
@SP
M=M+1

@54
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress392
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress392)

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

@118
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress393
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress393)

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

@119
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress394
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress394)

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

@120
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
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

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@30
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress395
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress395)

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

@121
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@62
D=A
@SP
A=M
M=D
@SP
M=M+1

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

@24
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress396
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress396)

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

@122
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@27
D=A
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

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@51
D=A
@SP
A=M
M=D
@SP
M=M+1

@63
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress397
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress397)

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

@123
D=A
@SP
A=M
M=D
@SP
M=M+1

@56
D=A
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

@12
D=A
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

@7
D=A
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

@12
D=A
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

@56
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress398
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress398)

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

@124
D=A
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

@12
D=A
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

@12
D=A
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

@12
D=A
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

@12
D=A
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress399
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress399)

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

@125
D=A
@SP
A=M
M=D
@SP
M=M+1

@7
D=A
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

@12
D=A
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

@56
D=A
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

@12
D=A
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

@7
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress400
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress400)

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

@126
D=A
@SP
A=M
M=D
@SP
M=M+1

@38
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@25
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.create.ReturnAddress401
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
@12
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Output.create
0;JMP

(Output.create.ReturnAddress401)

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

@0
D=A
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

(Output.create)

@1
D=A
@Output.create.End
D;JEQ
(Output.create.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.create.Loop
D=D-1;JNE
(Output.create.End)

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress403
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

@Array.new
0;JMP

(Array.new.ReturnAddress403)

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

@Output.5
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
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@5
D=A
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

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@2
D=A
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

@ARG
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@3
D=A
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

@ARG
D=M
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@4
D=A
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

@ARG
D=M
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@5
D=A
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

@ARG
D=M
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@6
D=A
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

@ARG
D=M
@7
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@7
D=A
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

@ARG
D=M
@8
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@8
D=A
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

@ARG
D=M
@9
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@9
D=A
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

@ARG
D=M
@10
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@10
D=A
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

@ARG
D=M
@11
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@0
D=A
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

(Output.createShiftedMap)

@4
D=A
@Output.createShiftedMap.End
D;JEQ
(Output.createShiftedMap.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.createShiftedMap.Loop
D=D-1;JNE
(Output.createShiftedMap.End)

@127
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress405
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

@Array.new
0;JMP

(Array.new.ReturnAddress405)

@SP
A=M-1
D=M
@Output.6
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

(Output.WHILE_EXP0)

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

@127
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
@TRUE_47
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_47
0;JMP
(TRUE_47)
@SP
A=M-1
A=A-1
M=-1
(END_47)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Output.WHILE_END0
D;JNE

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

@Output.5
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

@11
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress406
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

@Array.new
0;JMP

(Array.new.ReturnAddress406)

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

@Output.6
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@0
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

(Output.WHILE_EXP1)

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

@11
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
@TRUE_48
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_48
0;JMP
(TRUE_48)
@SP
A=M-1
A=A-1
M=-1
(END_48)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Output.WHILE_END1
D;JNE

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

@256
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress407
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress407)

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

@5
D=A
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
@3
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

@Output.WHILE_EXP1
0;JMP

(Output.WHILE_END1)

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

@0
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
@TRUE_49
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_49
0;JMP
(TRUE_49)
@SP
A=M-1
A=A-1
M=-1
(END_49)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@32
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

@Output.IF_END0
0;JMP

(Output.IF_FALSE0)

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

(Output.IF_END0)

@Output.WHILE_EXP0
0;JMP

(Output.WHILE_END0)

@0
D=A
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

(Output.getMap)

@1
D=A
@Output.getMap.End
D;JEQ
(Output.getMap.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.getMap.Loop
D=D-1;JNE
(Output.getMap.End)

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

@32
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
@TRUE_50
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_50
0;JMP
(TRUE_50)
@SP
A=M-1
A=A-1
M=-1
(END_50)
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

@126
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
@TRUE_51
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_51
0;JMP
(TRUE_51)
@SP
A=M-1
A=A-1
M=-1
(END_51)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(Output.IF_FALSE0)

@Output.2
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
@Output.IF_TRUE1
D;JNE

@Output.IF_FALSE1
0;JMP

(Output.IF_TRUE1)

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

@Output.5
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

@Output.IF_END1
0;JMP

(Output.IF_FALSE1)

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

@Output.6
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

(Output.IF_END1)

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

(Output.drawChar)

@4
D=A
@Output.drawChar.End
D;JEQ
(Output.drawChar.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.drawChar.Loop
D=D-1;JNE
(Output.drawChar.End)

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

@Output.getMap.ReturnAddress410
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

@Output.getMap
0;JMP

(Output.getMap.ReturnAddress410)

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

@Output.1
D=M
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

(Output.WHILE_EXP0)

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

@11
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
@TRUE_52
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_52
0;JMP
(TRUE_52)
@SP
A=M-1
A=A-1
M=-1
(END_52)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Output.WHILE_END0
D;JNE

@Output.2
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
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

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

@Output.4
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

@256
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=-M

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

@Output.IF_END0
0;JMP

(Output.IF_FALSE0)

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

@Output.4
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

@255
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

(Output.IF_END0)

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

@Output.4
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

@5
D=A
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

@32
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

@Output.WHILE_EXP0
0;JMP

(Output.WHILE_END0)

@0
D=A
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

(Output.moveCursor)

@0
D=A
@Output.moveCursor.End
D;JEQ
(Output.moveCursor.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.moveCursor.Loop
D=D-1;JNE
(Output.moveCursor.End)

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

@0
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
@TRUE_53
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_53
0;JMP
(TRUE_53)
@SP
A=M-1
A=A-1
M=-1
(END_53)
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

@22
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
@TRUE_54
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_54
0;JMP
(TRUE_54)
@SP
A=M-1
A=A-1
M=-1
(END_54)
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

@0
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
@TRUE_55
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_55
0;JMP
(TRUE_55)
@SP
A=M-1
A=A-1
M=-1
(END_55)
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

@63
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
@TRUE_56
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_56
0;JMP
(TRUE_56)
@SP
A=M-1
A=A-1
M=-1
(END_56)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@20
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress412
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress412)

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

(Output.IF_FALSE0)

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

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress413
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress413)

@SP
A=M-1
D=M
@Output.0
M=D
@SP
M=M-1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@352
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress414
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress414)

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

@Output.0
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
D=M
@Output.1
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

@Output.0
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

@Math.multiply.ReturnAddress415
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress415)

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_57
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_57
0;JMP
(TRUE_57)
@SP
A=M-1
A=A-1
M=-1
(END_57)
@SP
M=M-1

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.drawChar.ReturnAddress416
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

@Output.drawChar
0;JMP

(Output.drawChar.ReturnAddress416)

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

@0
D=A
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

(Output.printChar)

@0
D=A
@Output.printChar.End
D;JEQ
(Output.printChar.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.printChar.Loop
D=D-1;JNE
(Output.printChar.End)

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

@String.newLine.ReturnAddress418
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

@String.newLine
0;JMP

(String.newLine.ReturnAddress418)

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_58
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_58
0;JMP
(TRUE_58)
@SP
A=M-1
A=A-1
M=-1
(END_58)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@Output.println.ReturnAddress419
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

@Output.println
0;JMP

(Output.println.ReturnAddress419)

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

@Output.IF_END0
0;JMP

(Output.IF_FALSE0)

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

@String.backSpace.ReturnAddress420
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

@String.backSpace
0;JMP

(String.backSpace.ReturnAddress420)

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_59
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_59
0;JMP
(TRUE_59)
@SP
A=M-1
A=A-1
M=-1
(END_59)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE1
D;JNE

@Output.IF_FALSE1
0;JMP

(Output.IF_TRUE1)

@Output.backSpace.ReturnAddress421
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

@Output.backSpace
0;JMP

(Output.backSpace.ReturnAddress421)

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

@Output.IF_END1
0;JMP

(Output.IF_FALSE1)

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

@Output.drawChar.ReturnAddress422
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

@Output.drawChar
0;JMP

(Output.drawChar.ReturnAddress422)

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

@Output.2
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE2
D;JNE

@Output.IF_FALSE2
0;JMP

(Output.IF_TRUE2)

@Output.0
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

@SP
A=M-1
D=M
@Output.0
M=D
@SP
M=M-1

@Output.1
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

@SP
A=M-1
D=M
@Output.1
M=D
@SP
M=M-1

(Output.IF_FALSE2)

@Output.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@32
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
@TRUE_60
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_60
0;JMP
(TRUE_60)
@SP
A=M-1
A=A-1
M=-1
(END_60)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE3
D;JNE

@Output.IF_FALSE3
0;JMP

(Output.IF_TRUE3)

@Output.println.ReturnAddress423
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

@Output.println
0;JMP

(Output.println.ReturnAddress423)

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

@Output.IF_END3
0;JMP

(Output.IF_FALSE3)

@Output.2
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

(Output.IF_END3)

(Output.IF_END1)

(Output.IF_END0)

@0
D=A
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

(Output.printString)

@2
D=A
@Output.printString.End
D;JEQ
(Output.printString.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.printString.Loop
D=D-1;JNE
(Output.printString.End)

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

@String.length.ReturnAddress425
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

@String.length
0;JMP

(String.length.ReturnAddress425)

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

(Output.WHILE_EXP0)

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
D=M
A=A-1
D=M-D
@TRUE_61
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_61
0;JMP
(TRUE_61)
@SP
A=M-1
A=A-1
M=-1
(END_61)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Output.WHILE_END0
D;JNE

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

@String.charAt.ReturnAddress426
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.charAt
0;JMP

(String.charAt.ReturnAddress426)

@Output.printChar.ReturnAddress427
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress427)

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

@Output.WHILE_EXP0
0;JMP

(Output.WHILE_END0)

@0
D=A
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

(Output.printInt)

@0
D=A
@Output.printInt.End
D;JEQ
(Output.printInt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.printInt.Loop
D=D-1;JNE
(Output.printInt.End)

@Output.3
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@String.setInt.ReturnAddress429
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@String.setInt
0;JMP

(String.setInt.ReturnAddress429)

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

@Output.3
D=M
@SP
A=M
M=D
@SP
M=M+1

@Output.printString.ReturnAddress430
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

@Output.printString
0;JMP

(Output.printString.ReturnAddress430)

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

@0
D=A
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

(Output.println)

@0
D=A
@Output.println.End
D;JEQ
(Output.println.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.println.Loop
D=D-1;JNE
(Output.println.End)

@Output.1
D=M
@SP
A=M
M=D
@SP
M=M+1

@352
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

@Output.0
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

@SP
A=M-1
D=M
@Output.1
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

@SP
A=M-1
D=M
@Output.0
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

@SP
A=M-1
M=!M

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

@Output.1
D=M
@SP
A=M
M=D
@SP
M=M+1

@8128
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
@TRUE_62
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_62
0;JMP
(TRUE_62)
@SP
A=M-1
A=A-1
M=-1
(END_62)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.1
M=D
@SP
M=M-1

(Output.IF_FALSE0)

@0
D=A
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

(Output.backSpace)

@0
D=A
@Output.backSpace.End
D;JEQ
(Output.backSpace.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Output.backSpace.Loop
D=D-1;JNE
(Output.backSpace.End)

@Output.2
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
@Output.IF_TRUE0
D;JNE

@Output.IF_FALSE0
0;JMP

(Output.IF_TRUE0)

@Output.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_63
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_63
0;JMP
(TRUE_63)
@SP
A=M-1
A=A-1
M=-1
(END_63)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE1
D;JNE

@Output.IF_FALSE1
0;JMP

(Output.IF_TRUE1)

@Output.0
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

@SP
A=M-1
D=M
@Output.0
M=D
@SP
M=M-1

@Output.1
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

@SP
A=M-1
D=M
@Output.1
M=D
@SP
M=M-1

@Output.IF_END1
0;JMP

(Output.IF_FALSE1)

@31
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.0
M=D
@SP
M=M-1

@Output.1
D=M
@SP
A=M
M=D
@SP
M=M+1

@32
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
@TRUE_64
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_64
0;JMP
(TRUE_64)
@SP
A=M-1
A=A-1
M=-1
(END_64)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Output.IF_TRUE2
D;JNE

@Output.IF_FALSE2
0;JMP

(Output.IF_TRUE2)

@8128
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.1
M=D
@SP
M=M-1

(Output.IF_FALSE2)

@Output.1
D=M
@SP
A=M
M=D
@SP
M=M+1

@321
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

@SP
A=M-1
D=M
@Output.1
M=D
@SP
M=M-1

(Output.IF_END1)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

@Output.IF_END0
0;JMP

(Output.IF_FALSE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@SP
A=M-1
D=M
@Output.2
M=D
@SP
M=M-1

(Output.IF_END0)

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.drawChar.ReturnAddress433
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

@Output.drawChar
0;JMP

(Output.drawChar.ReturnAddress433)

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

@0
D=A
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

(Screen.init)

@1
D=A
@Screen.init.End
D;JEQ
(Screen.init.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.init.Loop
D=D-1;JNE
(Screen.init.End)

@16384
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@Screen.1
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

@SP
A=M-1
M=!M

@SP
A=M-1
D=M
@Screen.2
M=D
@SP
M=M-1

@17
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress435
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

@Array.new
0;JMP

(Array.new.ReturnAddress435)

@SP
A=M-1
D=M
@Screen.0
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

@Screen.0
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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

(Screen.WHILE_EXP0)

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

@16
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
@TRUE_65
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_65
0;JMP
(TRUE_65)
@SP
A=M-1
A=A-1
M=-1
(END_65)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
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

@Screen.0
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

@Screen.0
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

@Screen.0
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

@5
D=A
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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

@0
D=A
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

(Screen.clearScreen)

@1
D=A
@Screen.clearScreen.End
D;JEQ
(Screen.clearScreen.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.clearScreen.Loop
D=D-1;JNE
(Screen.clearScreen.End)

(Screen.WHILE_EXP0)

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

@8192
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
@TRUE_66
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_66
0;JMP
(TRUE_66)
@SP
A=M-1
A=A-1
M=-1
(END_66)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
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

@Screen.1
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

@0
D=A
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

(Screen.updateLocation)

@0
D=A
@Screen.updateLocation.End
D;JEQ
(Screen.updateLocation.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.updateLocation.Loop
D=D-1;JNE
(Screen.updateLocation.End)

@Screen.2
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
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

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

@Screen.1
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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.1
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

@5
D=A
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

@Screen.IF_END0
0;JMP

(Screen.IF_FALSE0)

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

@Screen.1
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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.1
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
M=!M

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

@5
D=A
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

(Screen.IF_END0)

@0
D=A
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

(Screen.setColor)

@0
D=A
@Screen.setColor.End
D;JEQ
(Screen.setColor.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.setColor.Loop
D=D-1;JNE
(Screen.setColor.End)

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
D=M
@Screen.2
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

(Screen.drawPixel)

@3
D=A
@Screen.drawPixel.End
D;JEQ
(Screen.drawPixel.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawPixel.Loop
D=D-1;JNE
(Screen.drawPixel.End)

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

@0
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
@TRUE_67
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_67
0;JMP
(TRUE_67)
@SP
A=M-1
A=A-1
M=-1
(END_67)
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

@511
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
@TRUE_68
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_68
0;JMP
(TRUE_68)
@SP
A=M-1
A=A-1
M=-1
(END_68)
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

@0
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
@TRUE_69
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_69
0;JMP
(TRUE_69)
@SP
A=M-1
A=A-1
M=-1
(END_69)
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

@255
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
@TRUE_70
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_70
0;JMP
(TRUE_70)
@SP
A=M-1
A=A-1
M=-1
(END_70)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

@7
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress440
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress440)

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

(Screen.IF_FALSE0)

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

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress441
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress441)

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

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress442
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress442)

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress443
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress443)

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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.0
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

@Screen.updateLocation.ReturnAddress444
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress444)

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

@0
D=A
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

(Screen.drawConditional)

@0
D=A
@Screen.drawConditional.End
D;JEQ
(Screen.drawConditional.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawConditional.Loop
D=D-1;JNE
(Screen.drawConditional.End)

@ARG
D=M
@2
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
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

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

@Screen.drawPixel.ReturnAddress446
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawPixel
0;JMP

(Screen.drawPixel.ReturnAddress446)

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

@Screen.IF_END0
0;JMP

(Screen.IF_FALSE0)

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

@Screen.drawPixel.ReturnAddress447
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawPixel
0;JMP

(Screen.drawPixel.ReturnAddress447)

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

(Screen.IF_END0)

@0
D=A
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

(Screen.drawLine)

@11
D=A
@Screen.drawLine.End
D;JEQ
(Screen.drawLine.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawLine.Loop
D=D-1;JNE
(Screen.drawLine.End)

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

@0
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
@TRUE_71
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_71
0;JMP
(TRUE_71)
@SP
A=M-1
A=A-1
M=-1
(END_71)
@SP
M=M-1

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@511
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
@TRUE_72
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_72
0;JMP
(TRUE_72)
@SP
A=M-1
A=A-1
M=-1
(END_72)
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

@0
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
@TRUE_73
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_73
0;JMP
(TRUE_73)
@SP
A=M-1
A=A-1
M=-1
(END_73)
@SP
M=M-1

@ARG
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@255
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
@TRUE_74
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_74
0;JMP
(TRUE_74)
@SP
A=M-1
A=A-1
M=-1
(END_74)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress449
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress449)

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

(Screen.IF_FALSE0)

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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
D=D-M
A=A-1
M=D
@SP
M=M-1

@Math.abs.ReturnAddress450
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress450)

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

@ARG
D=M
@3
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

@Math.abs.ReturnAddress451
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

@Math.abs
0;JMP

(Math.abs.ReturnAddress451)

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
@2
A=D+A
D=M
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
@TRUE_75
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_75
0;JMP
(TRUE_75)
@SP
A=M-1
A=A-1
M=-1
(END_75)
@SP
M=M-1

@LCL
D=M
@6
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
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@3
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

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_76
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_76
0;JMP
(TRUE_76)
@SP
A=M-1
A=A-1
M=-1
(END_76)
@SP
M=M-1

@LCL
D=M
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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
D=M
A=A-1
D=M-D
@TRUE_77
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_77
0;JMP
(TRUE_77)
@SP
A=M-1
A=A-1
M=-1
(END_77)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE1
D;JNE

@Screen.IF_FALSE1
0;JMP

(Screen.IF_TRUE1)

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
@4
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
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@ARG
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

@LCL
D=M
@4
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
@3
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
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
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

(Screen.IF_FALSE1)

@LCL
D=M
@6
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
@Screen.IF_TRUE2
D;JNE

@Screen.IF_FALSE2
0;JMP

(Screen.IF_TRUE2)

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
@4
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
@8
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

@ARG
D=M
@2
A=D+A
D=M
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
@TRUE_78
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_78
0;JMP
(TRUE_78)
@SP
A=M-1
A=A-1
M=-1
(END_78)
@SP
M=M-1

@LCL
D=M
@7
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

@Screen.IF_END2
0;JMP

(Screen.IF_FALSE2)

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
@8
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@3
A=D+A
D=M
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
@TRUE_79
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_79
0;JMP
(TRUE_79)
@SP
A=M-1
A=A-1
M=-1
(END_79)
@SP
M=M-1

@LCL
D=M
@7
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

(Screen.IF_END2)

@2
D=A
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

@Math.multiply.ReturnAddress452
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress452)

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

@2
D=A
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

@Math.multiply.ReturnAddress453
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress453)

@LCL
D=M
@9
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

@2
D=A
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

@Math.multiply.ReturnAddress454
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress454)

@LCL
D=M
@10
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
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.drawConditional.ReturnAddress455
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawConditional
0;JMP

(Screen.drawConditional.ReturnAddress455)

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

(Screen.WHILE_EXP0)

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
@8
A=D+A
D=M
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
@TRUE_80
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_80
0;JMP
(TRUE_80)
@SP
A=M-1
A=A-1
M=-1
(END_80)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
D;JNE

@LCL
D=M
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_81
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_81
0;JMP
(TRUE_81)
@SP
A=M-1
A=A-1
M=-1
(END_81)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE3
D;JNE

@Screen.IF_FALSE3
0;JMP

(Screen.IF_TRUE3)

@LCL
D=M
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@9
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
@5
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

@Screen.IF_END3
0;JMP

(Screen.IF_FALSE3)

@LCL
D=M
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@10
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
@5
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
@7
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
@Screen.IF_TRUE4
D;JNE

@Screen.IF_FALSE4
0;JMP

(Screen.IF_TRUE4)

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

@Screen.IF_END4
0;JMP

(Screen.IF_FALSE4)

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

(Screen.IF_END4)

(Screen.IF_END3)

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
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.drawConditional.ReturnAddress456
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawConditional
0;JMP

(Screen.drawConditional.ReturnAddress456)

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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

@0
D=A
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

(Screen.drawRectangle)

@9
D=A
@Screen.drawRectangle.End
D;JEQ
(Screen.drawRectangle.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawRectangle.Loop
D=D-1;JNE
(Screen.drawRectangle.End)

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

@ARG
D=M
@2
A=D+A
D=M
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
@TRUE_82
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_82
0;JMP
(TRUE_82)
@SP
A=M-1
A=A-1
M=-1
(END_82)
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

@ARG
D=M
@3
A=D+A
D=M
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
@TRUE_83
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_83
0;JMP
(TRUE_83)
@SP
A=M-1
A=A-1
M=-1
(END_83)
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

@0
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
@TRUE_84
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_84
0;JMP
(TRUE_84)
@SP
A=M-1
A=A-1
M=-1
(END_84)
@SP
M=M-1

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@511
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
@TRUE_85
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_85
0;JMP
(TRUE_85)
@SP
A=M-1
A=A-1
M=-1
(END_85)
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

@0
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
@TRUE_86
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_86
0;JMP
(TRUE_86)
@SP
A=M-1
A=A-1
M=-1
(END_86)
@SP
M=M-1

@ARG
D=M
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@255
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
@TRUE_87
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_87
0;JMP
(TRUE_87)
@SP
A=M-1
A=A-1
M=-1
(END_87)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

@9
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress458
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress458)

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

(Screen.IF_FALSE0)

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

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress459
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress459)

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
@3
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress460
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress460)

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
@7
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
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress461
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress461)

@LCL
D=M
@4
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
@4
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress462
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress462)

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
@8
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
@7
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.0
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

@SP
A=M-1
M=!M

@LCL
D=M
@6
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
@8
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

@Screen.0
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

@LCL
D=M
@5
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress463
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress463)

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

(Screen.WHILE_EXP0)

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

@ARG
D=M
@3
A=D+A
D=M
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
@TRUE_88
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_88
0;JMP
(TRUE_88)
@SP
A=M-1
A=A-1
M=-1
(END_88)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
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

@0
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
@TRUE_89
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_89
0;JMP
(TRUE_89)
@SP
A=M-1
A=A-1
M=-1
(END_89)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE1
D;JNE

@Screen.IF_FALSE1
0;JMP

(Screen.IF_TRUE1)

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
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.updateLocation.ReturnAddress464
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress464)

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

@Screen.IF_END1
0;JMP

(Screen.IF_FALSE1)

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
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.updateLocation.ReturnAddress465
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress465)

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

(Screen.WHILE_EXP1)

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
D=M
A=A-1
D=M-D
@TRUE_90
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_90
0;JMP
(TRUE_90)
@SP
A=M-1
A=A-1
M=-1
(END_90)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END1
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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=-M

@Screen.updateLocation.ReturnAddress466
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress466)

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

@Screen.WHILE_EXP1
0;JMP

(Screen.WHILE_END1)

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
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.updateLocation.ReturnAddress467
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress467)

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

(Screen.IF_END1)

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

@ARG
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

@32
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
@2
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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

@0
D=A
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

(Screen.drawHorizontal)

@11
D=A
@Screen.drawHorizontal.End
D;JEQ
(Screen.drawHorizontal.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawHorizontal.Loop
D=D-1;JNE
(Screen.drawHorizontal.End)

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

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.min.ReturnAddress469
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.min
0;JMP

(Math.min.ReturnAddress469)

@LCL
D=M
@7
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
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Math.max.ReturnAddress470
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.max
0;JMP

(Math.max.ReturnAddress470)

@LCL
D=M
@8
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
M=-M

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_91
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_91
0;JMP
(TRUE_91)
@SP
A=M-1
A=A-1
M=-1
(END_91)
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

@256
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
@TRUE_92
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_92
0;JMP
(TRUE_92)
@SP
A=M-1
A=A-1
M=-1
(END_92)
@SP
M=M-1

@LCL
D=M
@7
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@512
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
@TRUE_93
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_93
0;JMP
(TRUE_93)
@SP
A=M-1
A=A-1
M=-1
(END_93)
@SP
M=M-1

@LCL
D=M
@8
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
M=-M

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_94
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_94
0;JMP
(TRUE_94)
@SP
A=M-1
A=A-1
M=-1
(END_94)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

@LCL
D=M
@7
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.max.ReturnAddress471
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.max
0;JMP

(Math.max.ReturnAddress471)

@LCL
D=M
@7
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
@8
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@511
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.min.ReturnAddress472
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.min
0;JMP

(Math.min.ReturnAddress472)

@LCL
D=M
@8
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
@7
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress473
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress473)

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

@LCL
D=M
@7
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

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress474
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress474)

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
@9
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
@8
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress475
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress475)

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

@LCL
D=M
@8
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

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress476
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress476)

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
@10
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
@9
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.0
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

@SP
A=M-1
M=!M

@LCL
D=M
@5
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
@10
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

@Screen.0
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

@LCL
D=M
@4
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

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress477
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress477)

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
@6
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
@6
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

@LCL
D=M
@6
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_95
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_95
0;JMP
(TRUE_95)
@SP
A=M-1
A=A-1
M=-1
(END_95)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE1
D;JNE

@Screen.IF_FALSE1
0;JMP

(Screen.IF_TRUE1)

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
@4
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
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.updateLocation.ReturnAddress478
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress478)

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

@Screen.IF_END1
0;JMP

(Screen.IF_FALSE1)

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
@5
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Screen.updateLocation.ReturnAddress479
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress479)

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

(Screen.WHILE_EXP0)

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
@3
A=D+A
D=M
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
@TRUE_96
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_96
0;JMP
(TRUE_96)
@SP
A=M-1
A=A-1
M=-1
(END_96)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=-M

@Screen.updateLocation.ReturnAddress480
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress480)

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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

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

@Screen.updateLocation.ReturnAddress481
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.updateLocation
0;JMP

(Screen.updateLocation.ReturnAddress481)

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

(Screen.IF_END1)

(Screen.IF_FALSE0)

@0
D=A
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

(Screen.drawSymetric)

@0
D=A
@Screen.drawSymetric.End
D;JEQ
(Screen.drawSymetric.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawSymetric.Loop
D=D-1;JNE
(Screen.drawSymetric.End)

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

@ARG
D=M
@3
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

@ARG
D=M
@2
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
@2
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

@Screen.drawHorizontal.ReturnAddress483
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawHorizontal
0;JMP

(Screen.drawHorizontal.ReturnAddress483)

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

@ARG
D=M
@3
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
@2
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
@2
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

@Screen.drawHorizontal.ReturnAddress484
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawHorizontal
0;JMP

(Screen.drawHorizontal.ReturnAddress484)

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

@ARG
D=M
@2
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

@ARG
D=M
@3
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

@ARG
D=M
@3
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

@Screen.drawHorizontal.ReturnAddress485
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawHorizontal
0;JMP

(Screen.drawHorizontal.ReturnAddress485)

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

@ARG
D=M
@2
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
@3
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

@ARG
D=M
@3
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

@Screen.drawHorizontal.ReturnAddress486
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
@3
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawHorizontal
0;JMP

(Screen.drawHorizontal.ReturnAddress486)

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

@0
D=A
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

(Screen.drawCircle)

@3
D=A
@Screen.drawCircle.End
D;JEQ
(Screen.drawCircle.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Screen.drawCircle.Loop
D=D-1;JNE
(Screen.drawCircle.End)

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

@0
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
@TRUE_97
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_97
0;JMP
(TRUE_97)
@SP
A=M-1
A=A-1
M=-1
(END_97)
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

@511
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
@TRUE_98
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_98
0;JMP
(TRUE_98)
@SP
A=M-1
A=A-1
M=-1
(END_98)
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

@0
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
@TRUE_99
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_99
0;JMP
(TRUE_99)
@SP
A=M-1
A=A-1
M=-1
(END_99)
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

@255
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
@TRUE_100
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_100
0;JMP
(TRUE_100)
@SP
A=M-1
A=A-1
M=-1
(END_100)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE0
D;JNE

@Screen.IF_FALSE0
0;JMP

(Screen.IF_TRUE0)

@12
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress488
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress488)

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

(Screen.IF_FALSE0)

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

@ARG
D=M
@2
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

@0
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
@TRUE_101
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_101
0;JMP
(TRUE_101)
@SP
A=M-1
A=A-1
M=-1
(END_101)
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

@ARG
D=M
@2
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

@511
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
@TRUE_102
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_102
0;JMP
(TRUE_102)
@SP
A=M-1
A=A-1
M=-1
(END_102)
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

@ARG
D=M
@2
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

@0
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
@TRUE_103
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_103
0;JMP
(TRUE_103)
@SP
A=M-1
A=A-1
M=-1
(END_103)
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

@ARG
D=M
@2
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

@255
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
@TRUE_104
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_104
0;JMP
(TRUE_104)
@SP
A=M-1
A=A-1
M=-1
(END_104)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE1
D;JNE

@Screen.IF_FALSE1
0;JMP

(Screen.IF_TRUE1)

@13
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress489
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress489)

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

(Screen.IF_FALSE1)

@ARG
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

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@2
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

@Screen.drawSymetric.ReturnAddress490
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
@4
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawSymetric
0;JMP

(Screen.drawSymetric.ReturnAddress490)

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

(Screen.WHILE_EXP0)

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
D=M
A=A-1
D=M-D
@TRUE_105
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_105
0;JMP
(TRUE_105)
@SP
A=M-1
A=A-1
M=-1
(END_105)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Screen.WHILE_END0
D;JNE

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

@0
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
@TRUE_106
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_106
0;JMP
(TRUE_106)
@SP
A=M-1
A=A-1
M=-1
(END_106)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Screen.IF_TRUE2
D;JNE

@Screen.IF_FALSE2
0;JMP

(Screen.IF_TRUE2)

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

@2
D=A
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

@Math.multiply.ReturnAddress491
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress491)

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

@3
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

@Screen.IF_END2
0;JMP

(Screen.IF_FALSE2)

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

@2
D=A
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
D=D-M
A=A-1
M=D
@SP
M=M-1

@Math.multiply.ReturnAddress492
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress492)

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

@5
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

(Screen.IF_END2)

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

@Screen.drawSymetric.ReturnAddress493
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
@4
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Screen.drawSymetric
0;JMP

(Screen.drawSymetric.ReturnAddress493)

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

@Screen.WHILE_EXP0
0;JMP

(Screen.WHILE_END0)

@0
D=A
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

(String.new)

@0
D=A
@String.new.End
D;JEQ
(String.new.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.new.Loop
D=D-1;JNE
(String.new.End)

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.alloc.ReturnAddress495
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

@Memory.alloc
0;JMP

(Memory.alloc.ReturnAddress495)

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

@0
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
@TRUE_107
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_107
0;JMP
(TRUE_107)
@SP
A=M-1
A=A-1
M=-1
(END_107)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@14
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress496
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress496)

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

(String.IF_FALSE0)

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

@0
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
@TRUE_108
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_108
0;JMP
(TRUE_108)
@SP
A=M-1
A=A-1
M=-1
(END_108)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE1
D;JNE

@String.IF_FALSE1
0;JMP

(String.IF_TRUE1)

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

@Array.new.ReturnAddress497
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

@Array.new
0;JMP

(Array.new.ReturnAddress497)

@THIS
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

(String.IF_FALSE1)

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

@THIS
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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

(String.dispose)

@0
D=A
@String.dispose.End
D;JEQ
(String.dispose.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.dispose.Loop
D=D-1;JNE
(String.dispose.End)

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

@THIS
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_109
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_109
0;JMP
(TRUE_109)
@SP
A=M-1
A=A-1
M=-1
(END_109)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@THIS
D=M
@1
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Array.dispose.ReturnAddress499
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

@Array.dispose
0;JMP

(Array.dispose.ReturnAddress499)

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

(String.IF_FALSE0)

@THIS
D=A
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Memory.deAlloc.ReturnAddress500
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

@Memory.deAlloc
0;JMP

(Memory.deAlloc.ReturnAddress500)

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

@0
D=A
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

(String.length)

@0
D=A
@String.length.End
D;JEQ
(String.length.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.length.Loop
D=D-1;JNE
(String.length.End)

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

@THIS
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

(String.charAt)

@0
D=A
@String.charAt.End
D;JEQ
(String.charAt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.charAt.Loop
D=D-1;JNE
(String.charAt.End)

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

@0
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
@TRUE_110
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_110
0;JMP
(TRUE_110)
@SP
A=M-1
A=A-1
M=-1
(END_110)
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

@THIS
D=M
@2
A=D+A
D=M
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
@TRUE_111
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_111
0;JMP
(TRUE_111)
@SP
A=M-1
A=A-1
M=-1
(END_111)
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

@THIS
D=M
@2
A=D+A
D=M
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
@TRUE_112
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_112
0;JMP
(TRUE_112)
@SP
A=M-1
A=A-1
M=-1
(END_112)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress503
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress503)

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

(String.IF_FALSE0)

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

(String.setCharAt)

@0
D=A
@String.setCharAt.End
D;JEQ
(String.setCharAt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.setCharAt.Loop
D=D-1;JNE
(String.setCharAt.End)

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

@0
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
@TRUE_113
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_113
0;JMP
(TRUE_113)
@SP
A=M-1
A=A-1
M=-1
(END_113)
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

@THIS
D=M
@2
A=D+A
D=M
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
@TRUE_114
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_114
0;JMP
(TRUE_114)
@SP
A=M-1
A=A-1
M=-1
(END_114)
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

@THIS
D=M
@2
A=D+A
D=M
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
@TRUE_115
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_115
0;JMP
(TRUE_115)
@SP
A=M-1
A=A-1
M=-1
(END_115)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@16
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress505
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress505)

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

(String.IF_FALSE0)

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

@ARG
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@0
D=A
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

(String.appendChar)

@0
D=A
@String.appendChar.End
D;JEQ
(String.appendChar.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.appendChar.Loop
D=D-1;JNE
(String.appendChar.End)

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

@THIS
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
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
D=M
A=A-1
D=M-D
@TRUE_116
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_116
0;JMP
(TRUE_116)
@SP
A=M-1
A=A-1
M=-1
(END_116)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@17
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress507
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress507)

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

(String.IF_FALSE0)

@THIS
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

@5
D=A
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

@THIS
D=M
@2
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

(String.eraseLastChar)

@0
D=A
@String.eraseLastChar.End
D;JEQ
(String.eraseLastChar.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.eraseLastChar.Loop
D=D-1;JNE
(String.eraseLastChar.End)

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

@THIS
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_117
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_117
0;JMP
(TRUE_117)
@SP
A=M-1
A=A-1
M=-1
(END_117)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@18
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress509
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress509)

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

(String.IF_FALSE0)

@THIS
D=M
@2
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

@THIS
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

@0
D=A
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

(String.intValue)

@5
D=A
@String.intValue.End
D;JEQ
(String.intValue.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.intValue.Loop
D=D-1;JNE
(String.intValue.End)

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

@THIS
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_118
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_118
0;JMP
(TRUE_118)
@SP
A=M-1
A=A-1
M=-1
(END_118)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@0
D=A
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

(String.IF_FALSE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

@45
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
@TRUE_119
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_119
0;JMP
(TRUE_119)
@SP
A=M-1
A=A-1
M=-1
(END_119)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE1
D;JNE

@String.IF_FALSE1
0;JMP

(String.IF_TRUE1)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@LCL
D=M
@4
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

(String.IF_FALSE1)

(String.WHILE_EXP0)

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

@THIS
D=M
@2
A=D+A
D=M
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
@TRUE_120
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_120
0;JMP
(TRUE_120)
@SP
A=M-1
A=A-1
M=-1
(END_120)
@SP
M=M-1

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

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@String.WHILE_END0
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

@THIS
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

@48
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

@0
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
@TRUE_121
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_121
0;JMP
(TRUE_121)
@SP
A=M-1
A=A-1
M=-1
(END_121)
@SP
M=M-1

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

@9
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
@TRUE_122
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_122
0;JMP
(TRUE_122)
@SP
A=M-1
A=A-1
M=-1
(END_122)
@SP
M=M-1

@SP
A=M-1
M=!M

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

@SP
M=M-1
A=M
D=M
@String.IF_TRUE2
D;JNE

@String.IF_FALSE2
0;JMP

(String.IF_TRUE2)

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

@10
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress511
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress511)

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

(String.IF_FALSE2)

@String.WHILE_EXP0
0;JMP

(String.WHILE_END0)

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
M=M-1
A=M
D=M
@String.IF_TRUE3
D;JNE

@String.IF_FALSE3
0;JMP

(String.IF_TRUE3)

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
M=-M

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

(String.IF_FALSE3)

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

(String.setInt)

@4
D=A
@String.setInt.End
D;JEQ
(String.setInt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.setInt.Loop
D=D-1;JNE
(String.setInt.End)

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

@THIS
D=M
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@0
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
@TRUE_123
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_123
0;JMP
(TRUE_123)
@SP
A=M-1
A=A-1
M=-1
(END_123)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE0
D;JNE

@String.IF_FALSE0
0;JMP

(String.IF_TRUE0)

@19
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress513
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress513)

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

(String.IF_FALSE0)

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@Array.new.ReturnAddress514
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

@Array.new
0;JMP

(Array.new.ReturnAddress514)

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

@0
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
@TRUE_124
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_124
0;JMP
(TRUE_124)
@SP
A=M-1
A=A-1
M=-1
(END_124)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE1
D;JNE

@String.IF_FALSE1
0;JMP

(String.IF_TRUE1)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

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
M=-M

@ARG
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

(String.IF_FALSE1)

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

(String.WHILE_EXP0)

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

@0
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
@TRUE_125
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_125
0;JMP
(TRUE_125)
@SP
A=M-1
A=A-1
M=-1
(END_125)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@String.WHILE_END0
D;JNE

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

@10
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.divide.ReturnAddress515
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.divide
0;JMP

(Math.divide.ReturnAddress515)

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
@2
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

@48
D=A
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

@10
D=A
@SP
A=M
M=D
@SP
M=M+1

@Math.multiply.ReturnAddress516
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
@2
D=D-A
@5
D=D-A
@ARG
M=D

@SP
D=M
@LCL
M=D

@Math.multiply
0;JMP

(Math.multiply.ReturnAddress516)

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

@5
D=A
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

@ARG
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

@String.WHILE_EXP0
0;JMP

(String.WHILE_END0)

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

@SP
M=M-1
A=M
D=M
@String.IF_TRUE2
D;JNE

@String.IF_FALSE2
0;JMP

(String.IF_TRUE2)

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
@2
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

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

(String.IF_FALSE2)

@THIS
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
D=M
A=A-1
D=M-D
@TRUE_126
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_126
0;JMP
(TRUE_126)
@SP
A=M-1
A=A-1
M=-1
(END_126)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE3
D;JNE

@String.IF_FALSE3
0;JMP

(String.IF_TRUE3)

@19
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress517
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress517)

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

(String.IF_FALSE3)

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

@0
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
@TRUE_127
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_127
0;JMP
(TRUE_127)
@SP
A=M-1
A=A-1
M=-1
(END_127)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@String.IF_TRUE4
D;JNE

@String.IF_FALSE4
0;JMP

(String.IF_TRUE4)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

@48
D=A
@SP
A=M
M=D
@SP
M=M+1

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

@5
D=A
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

@THIS
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

@String.IF_END4
0;JMP

(String.IF_FALSE4)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

(String.WHILE_EXP1)

@THIS
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
D=M
A=A-1
D=M-D
@TRUE_128
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_128
0;JMP
(TRUE_128)
@SP
A=M-1
A=A-1
M=-1
(END_128)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@String.WHILE_END1
D;JNE

@THIS
D=M
@2
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
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

@THIS
D=M
@2
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
@2
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

@5
D=A
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

@THIS
D=M
@2
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

@String.WHILE_EXP1
0;JMP

(String.WHILE_END1)

(String.IF_END4)

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

@Array.dispose.ReturnAddress518
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

@Array.dispose
0;JMP

(Array.dispose.ReturnAddress518)

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

@0
D=A
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

(String.newLine)

@0
D=A
@String.newLine.End
D;JEQ
(String.newLine.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.newLine.Loop
D=D-1;JNE
(String.newLine.End)

@128
D=A
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

(String.backSpace)

@0
D=A
@String.backSpace.End
D;JEQ
(String.backSpace.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.backSpace.Loop
D=D-1;JNE
(String.backSpace.End)

@129
D=A
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

(String.doubleQuote)

@0
D=A
@String.doubleQuote.End
D;JEQ
(String.doubleQuote.Loop)
@SP
A=M
M=0
@SP
M=M+1
@String.doubleQuote.Loop
D=D-1;JNE
(String.doubleQuote.End)

@34
D=A
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

@Memory.init.ReturnAddress523
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

@Memory.init
0;JMP

(Memory.init.ReturnAddress523)

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

@Math.init.ReturnAddress524
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

@Math.init
0;JMP

(Math.init.ReturnAddress524)

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

@Screen.init.ReturnAddress525
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

@Screen.init
0;JMP

(Screen.init.ReturnAddress525)

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

@Output.init.ReturnAddress526
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

@Output.init
0;JMP

(Output.init.ReturnAddress526)

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

@Keyboard.init.ReturnAddress527
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

@Keyboard.init
0;JMP

(Keyboard.init.ReturnAddress527)

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

@Main.main.ReturnAddress528
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

@Main.main
0;JMP

(Main.main.ReturnAddress528)

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

@Sys.halt.ReturnAddress529
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

@Sys.halt
0;JMP

(Sys.halt.ReturnAddress529)

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

@0
D=A
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

(Sys.halt)

@0
D=A
@Sys.halt.End
D;JEQ
(Sys.halt.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.halt.Loop
D=D-1;JNE
(Sys.halt.End)

(Sys.WHILE_EXP0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Sys.WHILE_END0
D;JNE

@Sys.WHILE_EXP0
0;JMP

(Sys.WHILE_END0)

@0
D=A
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

(Sys.wait)

@1
D=A
@Sys.wait.End
D;JEQ
(Sys.wait.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.wait.Loop
D=D-1;JNE
(Sys.wait.End)

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

@0
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
@TRUE_129
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_129
0;JMP
(TRUE_129)
@SP
A=M-1
A=A-1
M=-1
(END_129)
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Sys.IF_TRUE0
D;JNE

@Sys.IF_FALSE0
0;JMP

(Sys.IF_TRUE0)

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.error.ReturnAddress532
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

@Sys.error
0;JMP

(Sys.error.ReturnAddress532)

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

(Sys.IF_FALSE0)

(Sys.WHILE_EXP0)

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

@0
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
@TRUE_130
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_130
0;JMP
(TRUE_130)
@SP
A=M-1
A=A-1
M=-1
(END_130)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Sys.WHILE_END0
D;JNE

@50
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

(Sys.WHILE_EXP1)

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

@0
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
@TRUE_131
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_131
0;JMP
(TRUE_131)
@SP
A=M-1
A=A-1
M=-1
(END_131)
@SP
M=M-1

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@Sys.WHILE_END1
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

@Sys.WHILE_EXP1
0;JMP

(Sys.WHILE_END1)

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

@Sys.WHILE_EXP0
0;JMP

(Sys.WHILE_END0)

@0
D=A
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

(Sys.error)

@0
D=A
@Sys.error.End
D;JEQ
(Sys.error.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Sys.error.Loop
D=D-1;JNE
(Sys.error.End)

@69
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.printChar.ReturnAddress534
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress534)

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

@82
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.printChar.ReturnAddress535
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress535)

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

@82
D=A
@SP
A=M
M=D
@SP
M=M+1

@Output.printChar.ReturnAddress536
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

@Output.printChar
0;JMP

(Output.printChar.ReturnAddress536)

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

@Output.printInt.ReturnAddress537
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

@Output.printInt
0;JMP

(Output.printInt.ReturnAddress537)

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

@Sys.halt.ReturnAddress538
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

@Sys.halt
0;JMP

(Sys.halt.ReturnAddress538)

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

@0
D=A
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


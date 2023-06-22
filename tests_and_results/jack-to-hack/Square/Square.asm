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

@1
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

@SquareGame.new.ReturnAddress28
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@SquareGame.new
0;JMP

(SquareGame.new.ReturnAddress28)

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

@SquareGame.run.ReturnAddress29
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@SquareGame.run
0;JMP

(SquareGame.run.ReturnAddress29)

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

@SquareGame.dispose.ReturnAddress30
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@SquareGame.dispose
0;JMP

(SquareGame.dispose.ReturnAddress30)

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

@Array.new.ReturnAddress32
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress32)

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

@Array.new.ReturnAddress33
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress33)

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
@TRUE_7
D;JLT
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
@TRUE_8
D;JLT
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
@TRUE_10
D;JGT
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
D;JGT
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
D;JLT
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

@Math.abs.ReturnAddress36
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress36)

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

@Math.abs.ReturnAddress37
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress37)

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
@TRUE_13
D;JLT
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
@TRUE_15
D;JEQ
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
@TRUE_16
D;JEQ
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

@Sys.error.ReturnAddress39
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress39)

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
@TRUE_17
D;JLT
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
D;JGT
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
D;JGT
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
D;JLT
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

@Math.abs.ReturnAddress40
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress40)

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

@Math.abs.ReturnAddress41
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress41)

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
@TRUE_21
D;JLT
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
@TRUE_23
D;JGT
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
D=D+A
@R13
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
@TRUE_24
D;JGT
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
@TRUE_26
D;JLT
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

@Sys.error.ReturnAddress43
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress43)

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

@Math.multiply.ReturnAddress44
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress44)

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
@TRUE_28
D;JGT
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
@TRUE_29
D;JLT
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
@TRUE_32
D;JLT
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

@Sys.error.ReturnAddress51
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress51)

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
@TRUE_33
D;JEQ
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
@TRUE_35
D;JLT
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
@TRUE_36
D;JEQ
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
@TRUE_37
D;JGT
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
@TRUE_39
D;JEQ
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
@TRUE_40
D;JGT
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

@Sys.error.ReturnAddress52
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress52)

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
@TRUE_41
D;JGT
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
@TRUE_42
D;JEQ
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
@TRUE_43
D;JEQ
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

@String.new.ReturnAddress55
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(String.new.ReturnAddress55)

@SP
A=M-1
D=M
@Output.3
M=D
@SP
M=M-1

@Output.initMap.ReturnAddress56
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.initMap.ReturnAddress56)

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

@Output.createShiftedMap.ReturnAddress57
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.createShiftedMap.ReturnAddress57)

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

@Array.new.ReturnAddress59
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress59)

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

@Output.create.ReturnAddress60
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress60)

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

@Output.create.ReturnAddress61
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress61)

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

@Output.create.ReturnAddress62
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress62)

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

@Output.create.ReturnAddress63
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress63)

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

@Output.create.ReturnAddress64
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress64)

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

@Output.create.ReturnAddress65
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress65)

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

@Output.create.ReturnAddress66
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress66)

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

@Output.create.ReturnAddress67
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress67)

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

@Output.create.ReturnAddress68
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress68)

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

@Output.create.ReturnAddress69
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress69)

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

@Output.create.ReturnAddress70
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress70)

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

@Output.create.ReturnAddress71
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress71)

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

@Output.create.ReturnAddress72
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress72)

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

@Output.create.ReturnAddress73
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress73)

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

@Output.create.ReturnAddress74
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress74)

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

@Output.create.ReturnAddress75
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress75)

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

@Output.create.ReturnAddress76
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress76)

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

@Output.create.ReturnAddress77
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress77)

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

@Output.create.ReturnAddress78
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress78)

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

@Output.create.ReturnAddress79
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress79)

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

@Output.create.ReturnAddress80
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress80)

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

@Output.create.ReturnAddress81
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress81)

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

@Output.create.ReturnAddress82
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress82)

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

@Output.create.ReturnAddress83
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress83)

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

@Output.create.ReturnAddress84
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress84)

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

@Output.create.ReturnAddress85
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress85)

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

@Output.create.ReturnAddress86
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress86)

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

@Output.create.ReturnAddress87
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress87)

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

@Output.create.ReturnAddress88
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress88)

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

@Output.create.ReturnAddress89
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress89)

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

@Output.create.ReturnAddress90
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress90)

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

@Output.create.ReturnAddress91
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress91)

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

@Output.create.ReturnAddress92
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress92)

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

@Output.create.ReturnAddress93
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress93)

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

@Output.create.ReturnAddress94
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress94)

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

@Output.create.ReturnAddress95
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress95)

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

@Output.create.ReturnAddress96
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress96)

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

@Output.create.ReturnAddress97
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress97)

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

@Output.create.ReturnAddress98
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress98)

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

@Output.create.ReturnAddress99
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress99)

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

@Output.create.ReturnAddress100
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress100)

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

@Output.create.ReturnAddress101
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress101)

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

@Output.create.ReturnAddress102
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress102)

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

@Output.create.ReturnAddress103
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress103)

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

@Output.create.ReturnAddress104
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress104)

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

@Output.create.ReturnAddress105
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress105)

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

@Output.create.ReturnAddress106
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress106)

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

@Output.create.ReturnAddress107
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress107)

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

@Output.create.ReturnAddress108
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress108)

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

@Output.create.ReturnAddress109
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress109)

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

@Output.create.ReturnAddress110
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress110)

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

@Output.create.ReturnAddress111
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress111)

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

@Output.create.ReturnAddress112
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress112)

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

@Output.create.ReturnAddress113
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress113)

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

@Output.create.ReturnAddress114
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress114)

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

@Output.create.ReturnAddress115
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress115)

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

@Output.create.ReturnAddress116
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress116)

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

@Output.create.ReturnAddress117
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress117)

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

@Output.create.ReturnAddress118
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress118)

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

@Output.create.ReturnAddress119
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress119)

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

@Output.create.ReturnAddress120
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress120)

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

@Output.create.ReturnAddress121
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress121)

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

@Output.create.ReturnAddress122
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress122)

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

@Output.create.ReturnAddress123
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress123)

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

@Output.create.ReturnAddress124
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress124)

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

@Output.create.ReturnAddress125
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress125)

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

@Output.create.ReturnAddress126
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress126)

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

@Output.create.ReturnAddress127
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress127)

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

@Output.create.ReturnAddress128
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress128)

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

@Output.create.ReturnAddress129
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress129)

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

@Output.create.ReturnAddress130
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress130)

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

@Output.create.ReturnAddress131
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress131)

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

@Output.create.ReturnAddress132
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress132)

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

@Output.create.ReturnAddress133
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress133)

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

@Output.create.ReturnAddress134
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress134)

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

@Output.create.ReturnAddress135
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress135)

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

@Output.create.ReturnAddress136
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress136)

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

@Output.create.ReturnAddress137
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress137)

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

@Output.create.ReturnAddress138
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress138)

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

@Output.create.ReturnAddress139
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress139)

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

@Output.create.ReturnAddress140
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress140)

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

@Output.create.ReturnAddress141
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress141)

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

@Output.create.ReturnAddress142
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress142)

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

@Output.create.ReturnAddress143
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress143)

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

@Output.create.ReturnAddress144
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress144)

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

@Output.create.ReturnAddress145
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress145)

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

@Output.create.ReturnAddress146
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress146)

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

@Output.create.ReturnAddress147
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress147)

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

@Output.create.ReturnAddress148
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress148)

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

@Output.create.ReturnAddress149
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress149)

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

@Output.create.ReturnAddress150
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress150)

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

@Output.create.ReturnAddress151
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress151)

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

@Output.create.ReturnAddress152
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress152)

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

@Output.create.ReturnAddress153
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress153)

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

@Output.create.ReturnAddress154
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress154)

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

@Output.create.ReturnAddress155
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.create.ReturnAddress155)

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

@Array.new.ReturnAddress157
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress157)

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

@Array.new.ReturnAddress159
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress159)

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
@TRUE_45
D;JLT
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

@Array.new.ReturnAddress160
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress160)

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
@TRUE_46
D;JLT
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

@Math.multiply.ReturnAddress161
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress161)

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
@TRUE_47
D;JEQ
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
@TRUE_49
D;JGT
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

@Output.getMap.ReturnAddress164
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.getMap.ReturnAddress164)

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
@TRUE_51
D;JLT
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
@TRUE_52
D;JGT
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

@Sys.error.ReturnAddress166
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress166)

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

@Math.divide.ReturnAddress167
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress167)

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

@Math.multiply.ReturnAddress168
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress168)

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

@Math.multiply.ReturnAddress169
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress169)

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_55
D;JEQ
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

@Output.drawChar.ReturnAddress170
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.drawChar.ReturnAddress170)

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

@String.newLine.ReturnAddress172
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(String.newLine.ReturnAddress172)

@SP
A=M-1
D=M
A=A-1
D=M-D
@TRUE_56
D;JEQ
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

@Output.println.ReturnAddress173
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.println.ReturnAddress173)

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

@String.backSpace.ReturnAddress174
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(String.backSpace.ReturnAddress174)

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
M=M-1
A=M
D=M
@Output.IF_TRUE1
D;JNE

@Output.IF_FALSE1
0;JMP

(Output.IF_TRUE1)

@Output.backSpace.ReturnAddress175
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.backSpace.ReturnAddress175)

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

@Output.drawChar.ReturnAddress176
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.drawChar.ReturnAddress176)

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
@Output.IF_TRUE3
D;JNE

@Output.IF_FALSE3
0;JMP

(Output.IF_TRUE3)

@Output.println.ReturnAddress177
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.println.ReturnAddress177)

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

@String.length.ReturnAddress179
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(String.length.ReturnAddress179)

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
@TRUE_59
D;JLT
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

@String.charAt.ReturnAddress180
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(String.charAt.ReturnAddress180)

@Output.printChar.ReturnAddress181
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printChar.ReturnAddress181)

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

@String.setInt.ReturnAddress183
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(String.setInt.ReturnAddress183)

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

@Output.printString.ReturnAddress184
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printString.ReturnAddress184)

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
@TRUE_61
D;JGT
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

@Output.drawChar.ReturnAddress187
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.drawChar.ReturnAddress187)

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

@Array.new.ReturnAddress189
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress189)

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
@TRUE_63
D;JLT
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
@TRUE_64
D;JLT
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
@TRUE_66
D;JGT
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

@Sys.error.ReturnAddress194
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress194)

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

@Math.divide.ReturnAddress195
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress195)

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

@Math.multiply.ReturnAddress196
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress196)

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

@Math.multiply.ReturnAddress197
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress197)

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

@Screen.updateLocation.ReturnAddress198
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress198)

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

@Screen.drawPixel.ReturnAddress200
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.drawPixel.ReturnAddress200)

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

@Screen.drawPixel.ReturnAddress201
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.drawPixel.ReturnAddress201)

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

@Sys.error.ReturnAddress203
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress203)

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

@Math.abs.ReturnAddress204
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress204)

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

@Math.abs.ReturnAddress205
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.abs.ReturnAddress205)

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
@TRUE_74
D;JLT
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
@TRUE_76
D;JGT
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
@TRUE_77
D;JGT
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

@Math.multiply.ReturnAddress206
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress206)

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

@Math.multiply.ReturnAddress207
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress207)

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

@Math.multiply.ReturnAddress208
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress208)

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

@Screen.drawConditional.ReturnAddress209
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawConditional.ReturnAddress209)

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
@TRUE_78
D;JLT
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
@TRUE_79
D;JLT
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

@Screen.drawConditional.ReturnAddress210
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawConditional.ReturnAddress210)

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
@TRUE_80
D;JGT
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
@TRUE_81
D;JGT
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
@TRUE_82
D;JLT
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

@Sys.error.ReturnAddress212
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress212)

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

@Math.divide.ReturnAddress213
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress213)

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

@Math.multiply.ReturnAddress214
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress214)

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

@Math.divide.ReturnAddress215
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress215)

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

@Math.multiply.ReturnAddress216
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress216)

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

@Math.multiply.ReturnAddress217
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress217)

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
@TRUE_86
D;JGT
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
@TRUE_87
D;JEQ
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

@Screen.updateLocation.ReturnAddress218
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress218)

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

@Screen.updateLocation.ReturnAddress219
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress219)

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
@TRUE_88
D;JLT
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

@Screen.updateLocation.ReturnAddress220
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress220)

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

@Screen.updateLocation.ReturnAddress221
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress221)

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

@Math.min.ReturnAddress223
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.min.ReturnAddress223)

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

@Math.max.ReturnAddress224
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.max.ReturnAddress224)

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
@TRUE_89
D;JGT
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
@TRUE_91
D;JLT
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
@TRUE_92
D;JGT
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

@Math.max.ReturnAddress225
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.max.ReturnAddress225)

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

@Math.min.ReturnAddress226
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.min.ReturnAddress226)

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

@Math.divide.ReturnAddress227
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress227)

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

@Math.multiply.ReturnAddress228
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress228)

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

@Math.divide.ReturnAddress229
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress229)

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

@Math.multiply.ReturnAddress230
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress230)

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

@Math.multiply.ReturnAddress231
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress231)

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
@TRUE_93
D;JEQ
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

@Screen.updateLocation.ReturnAddress232
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress232)

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

@Screen.updateLocation.ReturnAddress233
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress233)

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
@TRUE_94
D;JLT
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

@Screen.updateLocation.ReturnAddress234
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress234)

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

@Screen.updateLocation.ReturnAddress235
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Screen.updateLocation.ReturnAddress235)

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

@Screen.drawHorizontal.ReturnAddress237
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawHorizontal.ReturnAddress237)

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

@Screen.drawHorizontal.ReturnAddress238
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawHorizontal.ReturnAddress238)

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

@Screen.drawHorizontal.ReturnAddress239
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawHorizontal.ReturnAddress239)

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

@Screen.drawHorizontal.ReturnAddress240
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawHorizontal.ReturnAddress240)

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
@TRUE_95
D;JLT
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
@TRUE_96
D;JGT
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

@Sys.error.ReturnAddress242
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress242)

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

@Sys.error.ReturnAddress243
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress243)

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

@Screen.drawSymetric.ReturnAddress244
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawSymetric.ReturnAddress244)

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
@TRUE_103
D;JGT
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
@TRUE_104
D;JLT
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

@Math.multiply.ReturnAddress245
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress245)

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

@Math.multiply.ReturnAddress246
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress246)

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

@Screen.drawSymetric.ReturnAddress247
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.drawSymetric.ReturnAddress247)

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

(Square.new)

@0
D=A
@Square.new.End
D;JEQ
(Square.new.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.new.Loop
D=D-1;JNE
(Square.new.End)

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.alloc.ReturnAddress249
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.alloc.ReturnAddress249)

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
D=D+A
@R13
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

@Square.draw.ReturnAddress250
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.draw
0;JMP

(Square.draw.ReturnAddress250)

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

(Square.dispose)

@0
D=A
@Square.dispose.End
D;JEQ
(Square.dispose.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.dispose.Loop
D=D-1;JNE
(Square.dispose.End)

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

@Memory.deAlloc.ReturnAddress252
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.deAlloc.ReturnAddress252)

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

(Square.draw)

@0
D=A
@Square.draw.End
D;JEQ
(Square.draw.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.draw.Loop
D=D-1;JNE
(Square.draw.End)

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

@Screen.setColor.ReturnAddress254
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress254)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress255
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress255)

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

(Square.erase)

@0
D=A
@Square.erase.End
D;JEQ
(Square.erase.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.erase.Loop
D=D-1;JNE
(Square.erase.End)

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

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Screen.setColor.ReturnAddress257
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress257)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress258
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress258)

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

(Square.incSize)

@0
D=A
@Square.incSize.End
D;JEQ
(Square.incSize.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.incSize.Loop
D=D-1;JNE
(Square.incSize.End)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@254
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
@TRUE_105
D;JLT
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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@510
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
A=M-1
A=A-1
D=M
A=A+1
D=D&M
A=A-1
M=D
@SP
M=M-1

@SP
M=M-1
A=M
D=M
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

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

@Square.erase.ReturnAddress260
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.erase
0;JMP

(Square.erase.ReturnAddress260)

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

@Square.draw.ReturnAddress261
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.draw
0;JMP

(Square.draw.ReturnAddress261)

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

(Square.IF_FALSE0)

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

(Square.decSize)

@0
D=A
@Square.decSize.End
D;JEQ
(Square.decSize.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.decSize.Loop
D=D-1;JNE
(Square.decSize.End)

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
@TRUE_107
D;JGT
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
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

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

@Square.erase.ReturnAddress263
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.erase
0;JMP

(Square.erase.ReturnAddress263)

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

@Square.draw.ReturnAddress264
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.draw
0;JMP

(Square.draw.ReturnAddress264)

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

(Square.IF_FALSE0)

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

(Square.moveUp)

@0
D=A
@Square.moveUp.End
D;JEQ
(Square.moveUp.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.moveUp.Loop
D=D-1;JNE
(Square.moveUp.End)

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
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Screen.setColor.ReturnAddress266
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress266)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress267
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress267)

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
D=D-M
A=A-1
M=D
@SP
M=M-1

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

@Screen.setColor.ReturnAddress268
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress268)

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

@Screen.drawRectangle.ReturnAddress269
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress269)

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

(Square.IF_FALSE0)

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

(Square.moveDown)

@0
D=A
@Square.moveDown.End
D;JEQ
(Square.moveDown.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.moveDown.Loop
D=D-1;JNE
(Square.moveDown.End)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@254
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
D;JLT
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
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Screen.setColor.ReturnAddress271
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress271)

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

@Screen.drawRectangle.ReturnAddress272
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress272)

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

@Screen.setColor.ReturnAddress273
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress273)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress274
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress274)

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

(Square.IF_FALSE0)

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

(Square.moveLeft)

@0
D=A
@Square.moveLeft.End
D;JEQ
(Square.moveLeft.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.moveLeft.Loop
D=D-1;JNE
(Square.moveLeft.End)

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

@1
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
D;JGT
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

@SP
M=M-1
A=M
D=M
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Screen.setColor.ReturnAddress276
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress276)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress277
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress277)

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

@SP
A=M-1
M=!M

@Screen.setColor.ReturnAddress278
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress278)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress279
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress279)

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

(Square.IF_FALSE0)

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

(Square.moveRight)

@0
D=A
@Square.moveRight.End
D;JEQ
(Square.moveRight.Loop)
@SP
A=M
M=0
@SP
M=M+1
@Square.moveRight.Loop
D=D-1;JNE
(Square.moveRight.End)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@510
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
@TRUE_111
D;JLT
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

@SP
M=M-1
A=M
D=M
@Square.IF_TRUE0
D;JNE

@Square.IF_FALSE0
0;JMP

(Square.IF_TRUE0)

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@Screen.setColor.ReturnAddress281
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress281)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress282
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress282)

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

@SP
A=M-1
M=!M

@Screen.setColor.ReturnAddress283
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.setColor
0;JMP

(Screen.setColor.ReturnAddress283)

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
A=A-1
D=M
A=A+1
D=D+M
A=A-1
M=D
@SP
M=M-1

@Screen.drawRectangle.ReturnAddress284
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Screen.drawRectangle
0;JMP

(Screen.drawRectangle.ReturnAddress284)

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

(Square.IF_FALSE0)

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

(SquareGame.new)

@0
D=A
@SquareGame.new.End
D;JEQ
(SquareGame.new.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SquareGame.new.Loop
D=D-1;JNE
(SquareGame.new.End)

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@Memory.alloc.ReturnAddress286
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.alloc.ReturnAddress286)

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

@Square.new.ReturnAddress287
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.new
0;JMP

(Square.new.ReturnAddress287)

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

(SquareGame.dispose)

@0
D=A
@SquareGame.dispose.End
D;JEQ
(SquareGame.dispose.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SquareGame.dispose.Loop
D=D-1;JNE
(SquareGame.dispose.End)

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

@Square.dispose.ReturnAddress289
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.dispose
0;JMP

(Square.dispose.ReturnAddress289)

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
@0
A=D+A
D=M
@SP
A=M
M=D
@SP
M=M+1

@Memory.deAlloc.ReturnAddress290
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.deAlloc.ReturnAddress290)

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

(SquareGame.moveSquare)

@0
D=A
@SquareGame.moveSquare.End
D;JEQ
(SquareGame.moveSquare.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SquareGame.moveSquare.Loop
D=D-1;JNE
(SquareGame.moveSquare.End)

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
@SquareGame.IF_TRUE0
D;JNE

@SquareGame.IF_FALSE0
0;JMP

(SquareGame.IF_TRUE0)

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

@Square.moveUp.ReturnAddress292
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.moveUp
0;JMP

(Square.moveUp.ReturnAddress292)

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

(SquareGame.IF_FALSE0)

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
@TRUE_113
D;JEQ
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

@SP
M=M-1
A=M
D=M
@SquareGame.IF_TRUE1
D;JNE

@SquareGame.IF_FALSE1
0;JMP

(SquareGame.IF_TRUE1)

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

@Square.moveDown.ReturnAddress293
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.moveDown
0;JMP

(Square.moveDown.ReturnAddress293)

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

(SquareGame.IF_FALSE1)

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

@3
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
@TRUE_114
D;JEQ
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

@SP
M=M-1
A=M
D=M
@SquareGame.IF_TRUE2
D;JNE

@SquareGame.IF_FALSE2
0;JMP

(SquareGame.IF_TRUE2)

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

@Square.moveLeft.ReturnAddress294
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.moveLeft
0;JMP

(Square.moveLeft.ReturnAddress294)

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

(SquareGame.IF_FALSE2)

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

@4
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
@SquareGame.IF_TRUE3
D;JNE

@SquareGame.IF_FALSE3
0;JMP

(SquareGame.IF_TRUE3)

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

@Square.moveRight.ReturnAddress295
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.moveRight
0;JMP

(Square.moveRight.ReturnAddress295)

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

(SquareGame.IF_FALSE3)

@5
D=A
@SP
A=M
M=D
@SP
M=M+1

@Sys.wait.ReturnAddress296
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Sys.wait
0;JMP

(Sys.wait.ReturnAddress296)

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

(SquareGame.run)

@2
D=A
@SquareGame.run.End
D;JEQ
(SquareGame.run.Loop)
@SP
A=M
M=0
@SP
M=M+1
@SquareGame.run.Loop
D=D-1;JNE
(SquareGame.run.End)

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
D=D+A
@R13
M=D
@SP
A=M-1
D=M
@R13
A=M
M=D
@SP
M=M-1

(SquareGame.WHILE_EXP0)

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
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@SquareGame.WHILE_END0
D;JNE

(SquareGame.WHILE_EXP1)

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
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@SquareGame.WHILE_END1
D;JNE

@Keyboard.keyPressed.ReturnAddress298
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Keyboard.keyPressed.ReturnAddress298)

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

@SquareGame.moveSquare.ReturnAddress299
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@SquareGame.moveSquare
0;JMP

(SquareGame.moveSquare.ReturnAddress299)

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

@SquareGame.WHILE_EXP1
0;JMP

(SquareGame.WHILE_END1)

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

@81
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
@SquareGame.IF_TRUE0
D;JNE

@SquareGame.IF_FALSE0
0;JMP

(SquareGame.IF_TRUE0)

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

(SquareGame.IF_FALSE0)

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

@90
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
@SquareGame.IF_TRUE1
D;JNE

@SquareGame.IF_FALSE1
0;JMP

(SquareGame.IF_TRUE1)

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

@Square.decSize.ReturnAddress300
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.decSize
0;JMP

(Square.decSize.ReturnAddress300)

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

(SquareGame.IF_FALSE1)

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

@88
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
@SquareGame.IF_TRUE2
D;JNE

@SquareGame.IF_FALSE2
0;JMP

(SquareGame.IF_TRUE2)

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

@Square.incSize.ReturnAddress301
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@Square.incSize
0;JMP

(Square.incSize.ReturnAddress301)

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

(SquareGame.IF_FALSE2)

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

@131
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
@TRUE_120
D;JEQ
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

@SP
M=M-1
A=M
D=M
@SquareGame.IF_TRUE3
D;JNE

@SquareGame.IF_FALSE3
0;JMP

(SquareGame.IF_TRUE3)

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(SquareGame.IF_FALSE3)

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

@133
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
D;JEQ
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

@SP
M=M-1
A=M
D=M
@SquareGame.IF_TRUE4
D;JNE

@SquareGame.IF_FALSE4
0;JMP

(SquareGame.IF_TRUE4)

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(SquareGame.IF_FALSE4)

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

@130
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
D;JEQ
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
M=M-1
A=M
D=M
@SquareGame.IF_TRUE5
D;JNE

@SquareGame.IF_FALSE5
0;JMP

(SquareGame.IF_TRUE5)

@3
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(SquareGame.IF_FALSE5)

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

@132
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
@SquareGame.IF_TRUE6
D;JNE

@SquareGame.IF_FALSE6
0;JMP

(SquareGame.IF_TRUE6)

@4
D=A
@SP
A=M
M=D
@SP
M=M+1

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

(SquareGame.IF_FALSE6)

(SquareGame.WHILE_EXP2)

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
@TRUE_124
D;JEQ
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
A=M-1
M=!M

@SP
A=M-1
M=!M

@SP
M=M-1
A=M
D=M
@SquareGame.WHILE_END2
D;JNE

@Keyboard.keyPressed.ReturnAddress302
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Keyboard.keyPressed.ReturnAddress302)

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

@SquareGame.moveSquare.ReturnAddress303
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

@SquareGame.moveSquare
0;JMP

(SquareGame.moveSquare.ReturnAddress303)

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

@SquareGame.WHILE_EXP2
0;JMP

(SquareGame.WHILE_END2)

@SquareGame.WHILE_EXP0
0;JMP

(SquareGame.WHILE_END0)

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

@Memory.alloc.ReturnAddress305
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.alloc.ReturnAddress305)

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
@TRUE_125
D;JLT
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

@Sys.error.ReturnAddress306
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress306)

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
@TRUE_126
D;JGT
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

@Array.new.ReturnAddress307
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress307)

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
@TRUE_127
D;JGT
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

@Array.dispose.ReturnAddress309
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.dispose.ReturnAddress309)

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

@Memory.deAlloc.ReturnAddress310
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.deAlloc.ReturnAddress310)

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
@TRUE_129
D;JGT
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
@TRUE_130
D;JEQ
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

@Sys.error.ReturnAddress313
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress313)

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
@TRUE_131
D;JLT
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
@TRUE_132
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_132
0;JMP
(TRUE_132)
@SP
A=M-1
A=A-1
M=-1
(END_132)
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
@TRUE_133
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_133
0;JMP
(TRUE_133)
@SP
A=M-1
A=A-1
M=-1
(END_133)
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

@Sys.error.ReturnAddress315
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress315)

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
@TRUE_134
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_134
0;JMP
(TRUE_134)
@SP
A=M-1
A=A-1
M=-1
(END_134)
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

@Sys.error.ReturnAddress317
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress317)

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
@TRUE_135
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_135
0;JMP
(TRUE_135)
@SP
A=M-1
A=A-1
M=-1
(END_135)
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

@Sys.error.ReturnAddress319
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress319)

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
@TRUE_136
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_136
0;JMP
(TRUE_136)
@SP
A=M-1
A=A-1
M=-1
(END_136)
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
@TRUE_137
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_137
0;JMP
(TRUE_137)
@SP
A=M-1
A=A-1
M=-1
(END_137)
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
@TRUE_138
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_138
0;JMP
(TRUE_138)
@SP
A=M-1
A=A-1
M=-1
(END_138)
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
@TRUE_139
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_139
0;JMP
(TRUE_139)
@SP
A=M-1
A=A-1
M=-1
(END_139)
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
@TRUE_140
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_140
0;JMP
(TRUE_140)
@SP
A=M-1
A=A-1
M=-1
(END_140)
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

@Math.multiply.ReturnAddress321
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress321)

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
@TRUE_141
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_141
0;JMP
(TRUE_141)
@SP
A=M-1
A=A-1
M=-1
(END_141)
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

@Sys.error.ReturnAddress323
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress323)

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

@Array.new.ReturnAddress324
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.new.ReturnAddress324)

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
@TRUE_142
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_142
0;JMP
(TRUE_142)
@SP
A=M-1
A=A-1
M=-1
(END_142)
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
@TRUE_143
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_143
0;JMP
(TRUE_143)
@SP
A=M-1
A=A-1
M=-1
(END_143)
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

@Math.divide.ReturnAddress325
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.divide.ReturnAddress325)

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

@Math.multiply.ReturnAddress326
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
D=M
@2
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

(Math.multiply.ReturnAddress326)

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
@TRUE_144
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_144
0;JMP
(TRUE_144)
@SP
A=M-1
A=A-1
M=-1
(END_144)
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

@Sys.error.ReturnAddress327
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress327)

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
@TRUE_145
D;JEQ
@SP
A=M-1
A=A-1
M=0
@END_145
0;JMP
(TRUE_145)
@SP
A=M-1
A=A-1
M=-1
(END_145)
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
@TRUE_146
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_146
0;JMP
(TRUE_146)
@SP
A=M-1
A=A-1
M=-1
(END_146)
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

@Array.dispose.ReturnAddress328
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Array.dispose.ReturnAddress328)

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

@Memory.init.ReturnAddress333
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Memory.init.ReturnAddress333)

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

@Math.init.ReturnAddress334
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Math.init.ReturnAddress334)

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

@Screen.init.ReturnAddress335
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Screen.init.ReturnAddress335)

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

@Output.init.ReturnAddress336
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.init.ReturnAddress336)

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

@Keyboard.init.ReturnAddress337
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Keyboard.init.ReturnAddress337)

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

@Main.main.ReturnAddress338
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Main.main.ReturnAddress338)

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

@Sys.halt.ReturnAddress339
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.halt.ReturnAddress339)

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
@TRUE_147
D;JLT
@SP
A=M-1
A=A-1
M=0
@END_147
0;JMP
(TRUE_147)
@SP
A=M-1
A=A-1
M=-1
(END_147)
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

@Sys.error.ReturnAddress342
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.error.ReturnAddress342)

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
@TRUE_148
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_148
0;JMP
(TRUE_148)
@SP
A=M-1
A=A-1
M=-1
(END_148)
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
@TRUE_149
D;JGT
@SP
A=M-1
A=A-1
M=0
@END_149
0;JMP
(TRUE_149)
@SP
A=M-1
A=A-1
M=-1
(END_149)
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

@Output.printChar.ReturnAddress344
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printChar.ReturnAddress344)

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

@Output.printChar.ReturnAddress345
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printChar.ReturnAddress345)

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

@Output.printChar.ReturnAddress346
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printChar.ReturnAddress346)

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

@Output.printInt.ReturnAddress347
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Output.printInt.ReturnAddress347)

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

@Sys.halt.ReturnAddress348
D=A
@SP
A=M
M=D
@SP
M=M+1

@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1

@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1

@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1

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

(Sys.halt.ReturnAddress348)

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


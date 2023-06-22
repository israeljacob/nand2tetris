#lang racket

(require "stage07.rkt" "stage08.rkt" "stage10.1.rkt" "stage10.2.rkt" "stage11.rkt")
(require racket/file)

; translate-vm-to-hack is a function that takes a string vm-instructions as input
; and returns a string of Hack assembly code that represents the equivalent machine code
; of the input vm-instructions in the Hack Virtual Machine.
(define (translate-vm-to-hack vm-instructions output-port folder-name vm-file-name)
  ;; Split the vm-instructions string into a list of words using space as a separator.
  (define words (string-split vm-instructions " "))
  
  ;; Use the case statement to determine what kind of VM command is being translated.
  
    ;; If the length of words is 1, it is an arithmetic command.
    (define command (list-ref words 0))
     (set! command (list-ref (string-split command "/r") 0))
     (set! command (list-ref (string-split command "/n") 0))
     (case command
       (("add") (binary-code "+"  output-port))
       (("add\r") (binary-code "+"  output-port))
       (("sub") (binary-code "-" output-port))
       (("sub\r") (binary-code "-" output-port))
       (("neg") (unary-code "-" output-port))
       (("neg\r") (unary-code "-" output-port))
       (("eq") (compare "JEQ" output-port))
       (("eq\r") (compare "JEQ" output-port))
       (("gt") (compare "JGT" output-port))
       (("gt\r") (compare "JGT" output-port))
       (("lt") (compare "JLT" output-port))
       (("lt\r") (compare "JLT" output-port))
       (("and") (binary-code "&" output-port))
       (("or") (binary-code "|" output-port))
       (("not") (unary-code "!" output-port))
       (("not\r") (unary-code "!" output-port))
       (("label") (define label-name (list-ref (string-split (list-ref words 1) "\r") 0)) (label vm-file-name label-name output-port))
       (("goto") (define label-name (list-ref (string-split (list-ref words 1) "\r") 0)) (goto vm-file-name label-name output-port))
       (("if-goto") (define label-name (list-ref (string-split (list-ref words 1) "\r") 0)) (if-goto vm-file-name label-name output-port))
       (("push") (push (list-ref words 1) (list-ref words 2) output-port vm-file-name))
       (("pop") (pup (list-ref words 1) (list-ref words 2) output-port vm-file-name))
       (("call") (call (list-ref words 1) (string->number (list-ref (string-split (list-ref words 2)"\r") 0)) output-port))
       (("function") (function (list-ref words 1) (list-ref words 2) output-port))
       (("return") (return output-port))
       (("return\r") (return output-port))))


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
(define (open-asm directory folder-name)
  (define output-port (open-output-file (string-append directory "\\" folder-name ".asm") #:exists 'append))
   (display "@256\nD=A\n@SP\nM=D\n\n" output-port)
   (call "Sys.init" 0 output-port)
   (close-output-port output-port))

(define (vm-file? file)
     (string=? (substring (path->string file)(- (string-length (path->string file) ) 3)) ".vm"))

(define (count-vm-files directory folder-name)
  (define vm-files (filter  vm-file? (directory-list directory)))
  (cond ( > (length vm-files) 1)
       (open-asm directory folder-name)))

; This function takes a directory and an output file as arguments.
(define (vm-process-directory directory folder-name)
   (count-vm-files directory folder-name)
   
  ; It uses directory-list to obtain a list of VM files in the directory, and then applies process-vm-file to each file in turn.
  (let ((vm-files (filter vm-file? (directory-list directory))))
    (for-each (lambda (vm-file) (process-vm-file directory vm-file folder-name)) vm-files)))

(define (jack-process-directory directory folder-name)
  ; It uses directory-list to obtain a list of JACK files in the directory, and then applies read-text to each file in turn.
  (define (jack-file? file)
     (string=? (substring (path->string file)(- (string-length (path->string file) ) 5)) ".jack"))
  (let ((jack-files (filter jack-file? (directory-list directory))))
    (for-each (lambda (jack-file) (read-text directory jack-file folder-name)) jack-files)))

; It uses directory-list to obtain a list of XML files in the directory, and then applies open-out-file to each file in turn.
(define (xmlT-process-directory directory folder-name)
  (define (xmlT-file? file)
     (string=? (substring (path->string file)(- (string-length (path->string file) ) 4)) ".xml"))
  (let ((xmlT-files (filter xmlT-file? (directory-list directory))))
    (for-each (lambda (xmlT-file) (open-out-file directory xmlT-file folder-name)) xmlT-files)))

(define (xml-process-directory directory folder-name)
  (define (xml-file? file)
     (and (string=? (substring (path->string file)(- (string-length (path->string file) ) 4)) ".xml")
          (not (string=? (substring (path->string file)(- (string-length (path->string file) ) 5)) "T.xml"))))
  (let ((xml-files (filter xml-file? (directory-list directory))))
    (for-each (lambda (xml-file) (create-vm-output-file directory xml-file folder-name)) xml-files)))


(define (copy-files source-dir destination-dir)
  (for-each (lambda (file)
              (copy-file (build-path source-dir file)
                         (build-path destination-dir file)))
            (directory-list source-dir)))
  


(display "Enter the pass to the tirectory\n")
(define directory (read-line))
(define list (string-split directory "\\"))
(define folder-name (list-ref list (- (length list) 1)))
(jack-process-directory directory folder-name)
(xmlT-process-directory directory folder-name)
(xml-process-directory directory folder-name)
(copy-files "C:\\Users\\yisra\\Desktop\\to_pass\\study\\lev\\year 3\\software languages\\nand2tetris\\tools\\OS" directory)
; Process the directory and write the output to the output file
(vm-process-directory directory folder-name)
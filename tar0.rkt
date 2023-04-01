; Mordechai Cohen 206958035
; Yisrael Jacob 316481514

#lang racket

; Define global variables for the total buys and sells
(define buys 0)
(define sells 0)

; This function takes an output port, a product string, an amount, and a price.
(define (HandleBuy output-port product amount price)
  ; It displays the string "### BUY" followed by the product name, followed by "###\n" to the output port.
  ; It also updates the global variable buys by adding the total amount spent on this purchase.
  (display (format "### BUY ~a ###\n ~a\n" product (* amount price)) output-port)
  (set! buys (+ buys (* amount price))))

; This function is similar to HandleBuy, but it displays the string "$$$ SELL" instead of "### BUY".
(define (HandleSell output-port product amount price)
  ; It displays the string "$$$ SELL" followed by the product name, followed by "$$$\n" to the output port.
  ; It also updates the global variable sells by adding the total amount earned on this sale.
  (display (format "$$$ SELL ~a $$$\n ~a\n" product (* amount price)) output-port)
  (set! sells (+ sells (* amount price))))

; This function takes a directory, a VM file, and an output file as arguments.
(define (process-vm-file directory vm-file output-file)
  ; It opens the input and output files and displays the name of the VM file (without the .vm extension)
  ; followed by a newline character to the output file.
  (define input-port (open-input-file (string-append directory "/" (path->string vm-file))))
  (define vm-file-name (path->string vm-file))
  (define output-port (open-output-file output-file #:exists 'append))
  (display (substring vm-file-name 0 (- (string-length vm-file-name) 3)) output-port )
  (display "\n" output-port)
  
  ; It then reads lines from the input file and processes them using HandleBuy or HandleSell depending on whether
  ; the line starts with "buy" or "sell". When it reaches the end of the input file, it closes the input and output files.
  (let loop ((line (read-line input-port)))
    (cond
      ; If the end of the input file is reached, close the input and output files.
      [(eof-object? line)
       (close-input-port input-port)
       (close-output-port output-port)]
      
      ; If the line starts with "buy", use HandleBuy to process it.
      [(cond
         [(regexp-match? (regexp-quote "buy") (substring line 0 3))
          (let ((parts (string-split line)))
            (HandleBuy output-port (list-ref parts 1) (string->number (list-ref parts 2)) (string->number (list-ref parts 3))))]
         
         ; If the line starts with "sell", use HandleSell to process it.
         [(regexp-match? (regexp-quote "sell") (substring line 0 4))
          (let ((parts (string-split line)))
            (HandleSell output-port (list-ref parts 1) (string->number (list-ref parts 2)) (string->number (list-ref parts 3))))])
       
       ; After processing the line, read the next line from the input file.
       (loop (read-line input-port))])))

; This function takes a directory and an output file as arguments.
(define (process-directory directory output-file)
  ; It uses directory-list to obtain a list of VM files in the directory, and then applies process-vm-file to each file in turn.
  (define (vm-file? file)
     (regexp-match? (regexp-quote ".vm") (path->string file)))
  (let ((vm-files (filter vm-file? (directory-list directory))))
    (for-each (lambda (vm-file) (process-vm-file directory vm-file output-file)) vm-files)))

(define (main directory output-file)
  ; Process the directory and write the output to the output file
  (process-directory directory output-file)

  ; Print the total buys and sells to the console
  (printf "Total buys: ~a\n" buys)
  (printf "Total sells: ~a\n" sells)

  ; Append the total buys to the output file
  (with-output-to-file output-file
    (lambda () (printf "\nTotal buys: ~a\n" buys))
    #:exists 'append)

  ; Append the total sells to the output file
  (with-output-to-file output-file
    (lambda () (printf "Total sells: ~a\n" sells))
    #:exists 'append))

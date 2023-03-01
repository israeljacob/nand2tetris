#lang racket

; This function takes an output port, a product string, an amount, and a price.
(define (HandleBuy output-port product amount price)
  ; It displays the string "### BUY" followed by the product name, followed by "###\n" to the output port.
  (display (format "### BUY ~a ###\n" product) output-port)
  ; It then displays the product of the amount and the price, followed by a newline character to the output port.
  (display (format "~a\n" (* amount price)) output-port))

; This function is similar to HandleBuy, but it displays the string "$$$ SELL" instead of "### BUY".
(define (HandleSell output-port product amount price)
  (display (format "$$$ SELL ~a $$$\n" product) output-port)
  (display (format "~a\n" (* amount price)) output-port))

; This function takes a directory, a VM file, and an output file as arguments.
(define (process-vm-file directory vm-file output-file)
  ; It opens the input and output files and displays the name of the VM file (without the .vm extension)
  ; followed by a newline character to the output file.
  (define input-port (open-input-file (string-append directory "\\" (path->string vm-file))))
  (define vm-file-name (path->string vm-file))
  (define output-port (open-output-file output-file #:exists 'append))
  (display (substring vm-file-name 0 (- (string-length vm-file-name) 3)) output-port )
  (display "\n" output-port)
  ; It then reads lines from the input file and processes them using HandleBuy or HandleSell depending on whether
  ; the line starts with "buy" or "sell". When it reaches the end of the input file, it closes the input and output files.
  (let loop ((line (read-line input-port)))
    (cond
      [(eof-object? line)
       (close-input-port input-port)
       (close-output-port output-port)]
      [(cond
         [(regexp-match? (regexp-quote "buy") (substring line 0 3))
          (let ((parts (string-split line)))
            (HandleBuy output-port (list-ref parts 1) (string->number (list-ref parts 2)) (string->number (list-ref parts 3))))]
         [(regexp-match? (regexp-quote "sell") (substring line 0 4))
          (let ((parts (string-split line)))
            (HandleSell output-port (list-ref parts 1) (string->number (list-ref parts 2)) (string->number (list-ref parts 3))))])
       (loop (read-line input-port))])))

; This function takes a directory and an output file as arguments.
(define (process-directory directory output-file)
  ; It uses directory-list to obtain a list of VM files in the directory, and then applies process-vm-file to each file in turn.
  (define (vm-file? file)
     (regexp-match? (regexp-quote ".vm") (path->string file)))
  (let ((vm-files (filter vm-file? (directory-list directory))))
    (for-each (lambda (vm-file) (process-vm-file directory vm-file output-file)) vm-files)))

(define (sum-file filename prefix)
  ; define variables for keeping track of the running total and whether
  ; we've reached the line with the prefix
  (define sum 0)
  (define after-line #f)
  ; open the file for reading
  (with-input-from-file filename
    (lambda ()
      ; read each line of the file
      (let loop ((line (read-line)))
        (cond
          ; if we reach the end of the file, return the total
          ((eof-object? line)
           sum)
          ; if we find a line with the given prefix, set the after-line
          ; flag to indicate that we should start adding up numbers
          ((regexp-match? prefix line)
           (set! after-line #t)
           (loop (read-line)))
          ; if we're currently after the line with the prefix, add up
          ; any numbers we find
          (after-line
           (set! sum (+ sum (string->number line)))
           (set! after-line #f)
           (loop (read-line)))
          ; if we haven't found the line with the prefix yet, keep
          ; looking for it
          (else
           (loop (read-line))))))))

(define (main directory output-file)
  ; Process the directory and write the output to the output file
  (process-directory directory output-file)

  ; Sum the "BUY" transactions from the output file and store the result in 'buys'
  (define buys (sum-file output-file "BUY"))

  ; Sum the "SELL" transactions from the output file and store the result in 'sells'
  (define sells (sum-file output-file "SELL"))

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

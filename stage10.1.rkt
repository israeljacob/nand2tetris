#lang racket

(require xml)
(require xml/xexpr)
(require xml/plist)
(provide read-text)

(define keywords '("class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return")) ; List of keywords in Jack
(define symbols '("{" "}" "[" "]" "(" ")" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"))

(define word "")

(define (read-text directory jack-file folder-name)
  (define input-port (open-input-file (string-append directory "/" (path->string jack-file))))
  (define jack-file-name (substring (path->string jack-file) 0 (- (string-length (path->string jack-file)) 5)))
  (define output-port (open-output-file (string-append directory "/" jack-file-name "T.xml") #:exists 'append))
  (display "<tokens>\n" output-port)
  
  (let loop ((char (read-char input-port)))
    (cond
      [(eof-object? char)
       (close-input-port input-port)
       (write-to-list word output-port)]
      [(string=? (string char) "/")
       (write-to-list word output-port)
       (set! word "")
       (check-comments input-port output-port)
       (loop (read-char input-port))]
      [(string=? (string char) "<")
       (write-to-list word output-port)
       (write-to-list "&lt;" output-port)
       (set! word "")
       (loop (read-char input-port))]
      [(string=? (string char) ">")
       (write-to-list word output-port)
       (write-to-list "&gt;" output-port)
       (set! word "")
       (loop (read-char input-port))]
      [(string=? (string char) "&")
       (write-to-list word output-port)
       (write-to-list "&amp;" output-port)
       (set! word "")
       (loop (read-char input-port))]
      [(char=? char #\")
       (write-to-list word output-port)
       (set! word "@")
       (collect-str input-port output-port)
       (loop (read-char input-port))]
      [(member (string char) symbols)
       (write-to-list word output-port)
       (write-to-list (string char) output-port)
       (set! word "")       
       (loop (read-char input-port))]
      [(char-whitespace? char)
       (write-to-list word output-port)
       (set! word "")
       (loop (read-char input-port))]
      [else
       (set! word (string-append word (string char)))
       (loop (read-char input-port))]))

   (display "</tokens>\n" output-port)
  (close-output-port output-port))

(define (write-to-list word output-port)
  (cond
    [(member word keywords)
     (display "<keyword> " output-port)
     (display word output-port)
     (display " </keyword>\n" output-port)]
    [(or (member word symbols) (equal? word "&lt;") (equal? word "&gt;") (equal? word "&quot;") (equal? word "&amp;"))
     (display "<symbol> " output-port)
     (display word output-port)
     (display " </symbol>\n" output-port)]
     [(regexp-match #rx"@[^\n\"]*@" word)
     (display "<stringConstant> " output-port)
     (display (substring word 1 (- (string-length word) 1)) output-port)
     (display " </stringConstant>\n" output-port)]
    [(regexp-match #rx"[_A-Za-z][_A-Za-z0-9]*" word)
     (display "<identifier> " output-port)
     (display word output-port)
     (display " </identifier>\n" output-port)]
    [(regexp-match #rx"[0-9]+" word)
     (display "<integerConstant> " output-port)
     (display word output-port)
     (display " </integerConstant>\n" output-port)]
    [else (void)]))

(define (check-comments input-port output-port)
  (define char (read-char input-port))
  (define next-char (peek-char input-port))
  (cond
      [(eof-object? char)
       (close-input-port input-port)]
      [(string=? (string char) "/")
       (read-line input-port)]
      [(and (string=? (string char) "*")
          (string=? (string next-char) "*"))
     (read-until-end-of-comment input-port)]
      [else
       (write-to-list "/" output-port)
       (cond
         [(string=? (string char) "<")
       (write-to-list "&lt;" output-port)
       (set! word "")]
      [(string=? (string char) ">")
       (write-to-list "&gt;" output-port)
       (set! word "")]
      [(string=? (string char) "&")
       (write-to-list "&amp;" output-port)
       (set! word "")]
      [(member (string char) symbols)
       (write-to-list (string char) output-port)
       (set! word "")]
      [(char=? char #\")
       (set! word "@")
       (collect-str input-port output-port)
       (set! word "")]
      [(char-whitespace? char)
       (set! word "")]
      [else
       (set! word (string-append word (string char)))])]))

(define (collect-str input-port output-port)
  (let loop ((char (read-char input-port)))
    (cond
       [(eof-object? char)
       (close-input-port input-port)
       (write-to-list word output-port)]
       [(char=? char #\")
        (set! word (string-append word "@"))
        (write-to-list word output-port)
         (set! word "")]
       [else
        (set! word (string-append word (string char)))
        (loop (read-char input-port))])))

(define (read-until-end-of-comment input-port)
  (define char (read-char input-port))
  (define next-char (peek-char input-port))
  (cond
    [(eof-object? char)
     (close-input-port input-port)]
    [(and (string=? (string char) "*")
          (string=? (string next-char) "/"))
     (read-char input-port) ; Consume the '/'
     (set! word "")]
    [else
     (read-until-end-of-comment input-port)]))


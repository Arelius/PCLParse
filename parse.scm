;; utility macros

(require srfi/9)

;;(require-extension riaxpander)

(define-syntax while
  (syntax-rules ()
    ((while cond) (letrec ((loop (lambda () (if cond (loop) #f)))) (loop)))
     ((while cond body ...) (letrec ((loop (lambda () (if cond (begin body ... (loop)) #f)))) (loop)))))

(define-syntax rec-lambda
  (syntax-rules ()
          ((rec-lambda (name args ...) body ...) (letrec ((name (lambda (args ...) body ...)))
                                                  name))))

;; stream handling

(define-record-type parse-stream
  (make-parse-stream file in-port read-stack)
  parse-stream?
  (file parse-stream-get-file)
  (in-port get-in-port set-in-port!)
  (read-stack get-read-stack set-read-stack!))

(define (open-parse-stream file) (make-parse-stream file (open-input-file file) '(0)))

(define (get-next-char in) 
  (set-read-stack! in (cons (+ 1 (car (get-read-stack in))) (cdr (get-read-stack in))))
  (read-char (get-in-port in)))

(define (push-stream-frame! in)
  (set-read-stack! in (cons (car (get-read-stack in)) (get-read-stack in))))

(define (discard-stream-frame! in)
  (set-read-stack! in (cdr (get-read-stack in)))
  (close-input-port (get-in-port in))
  (set-in-port! in (open-input-file (parse-stream-get-file in)))
  ((rec-lambda (rec-call i)
              (if (eq? (car (get-read-stack in)) i)
                  #f
                  (begin
                    (read-char (get-in-port in))
                    (rec-call (+ i 1))))) 0))


(define (merge-stream-frame! in)
  (set-read-stack! in (cons (car (get-read-stack in))
                            (cddr (get-read-stack in)))))

(define (call-with-parse-stack parser in)
  (push-stream-frame! in)
  (let ((out (parser in)))
    (if out
        (begin
          (merge-stream-frame! in)
          out)
        (begin
          (discard-stream-frame! in)
          out))))

;; primitive parsers

(define (parse-fail in) #f)
(define (parse-success in) #t)

(define (parse-char in) (get-next-char in))
(define (parse-a-char char in) (let ((c (parse-char in)))
                                 (if (eq? c char)
                                     c
                                     #f)))

(define (parse-any-char chars in)
  (if (null? chars)
      #f
      (let ((out (parse-a-char (car chars) in)))
        (if out
            out
            (parse-any-char (cdr chars) in)))))

(define (parse-whitespace in)
  (parse-any-char '(#\space #\tab #\newline) in))

(define (parse-str str in)
  (if (eq? (string-length str) 0)
      #t
      (if (parse-a-char (string-ref str 0) in)
          (if (parse-str (substring str 1 (string-length str)) in)
              str
              #f)
          #f)))

(define (parse-num-char in)
  (parse-any-char (string->list "0123456789") in))

;; parser combinators

(define (parse-context parser)
  (lambda (in) (call-with-parse-stack parser in)))

(define (parse-or . parser)
  (lambda (in)
    (if (not (null? parser))
        (let ((out (parse-context ((car parser) in))))
          (if out
              out
              ((apply parse-or (cdr parser)) in)))
        #f)))

(define (parse-* parser)
  (letrec ((parser-loop (lambda (in)
                          (let ((out (call-with-parse-stack parser in)))
                            (if out
                                (cons out (parser-loop in))
                                '())))))
           (lambda (in) (let ((out (parser-loop in)))
                          (if (eq? out '())
                              #f
                              out)))))

(define (parse-last . parser)
  (lambda (in)
    (let ((out ((car parser) in)))
      (if out
          (if (null? (cdr parser))
              out
              ((parse-last (cdr parser)) in))
          #f))))

(define (parse-sequence . parser)
  (lambda (in)
    (let* ((out ((car parser) in))
          (ret (if (and out (cdr parser))
               ((parse-sequence (cdr parser)) in)
               #f)))
      (if (and (cdr parser) (not ret))
          #f
          (cons out ret)))))

;; Additional parsers

(define (parse-num in)
  (let ((ret ((parse-sequence
                                  (parse-* parse-num-char)
                                  (lambda (in) (parse-a-char #\.))
                                  (parse-* parse-num-char)) in)))
    (if ret 
        (string->number (list->string ret))
        #f)))

(define (parse-int in)
  (let ((ret ((parse-* parse-num-char) in)))
    (if ret
        (string->number (list->string ret))
        #f)))

(define parse-eat-whitespace (parse-* parse-whitespace))

;; pcl parser

(define-record-type line
  (make-line type x y len)
  line?
  (type get-line-type set-line-type!)
  (x get-line-x set-line-x!)
  (y get-line-y set-line-y!)
  (len get-line-len set-line-len!))

(define parse-space-num (parse-last parse-eat-whitespace parse-num))

(define (parse-a-line str sym in) 
  (parse-eat-whitespace in)
  (parse-istring str)
  (make-line sym (parse-space-num in) (parse-space-num parse-num in) (parse-space-num in)))

(define (parse-line in) (parse-or
                         (lambda (in) (parse-a-line "hlin" 'hline in))
                         (lambda (in) (parse-a-line "vlin" 'vline in))))

(define (parse-command) (parse-or
                            parse-line
                            parse-box
                            parse-font
                            parse-text))

(define (parse-pcl) (parse-* parse-command))

(define test-in (open-parse-stream "test.pcl"))
(call-with-parse-stack (lambda (in) (parse-str "tesj" in)) test-in)
(call-with-parse-stack (lambda (in) (parse-str "test" in)) test-in)
(call-with-parse-stack (lambda (in) (parse-str "test" in)) test-in)
(parse-eat-whitespace test-in)
(call-with-parse-stack (lambda (in) (parse-whitespace in)) test-in)
(call-with-parse-stack parse-num test-in)
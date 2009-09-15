;; utility macros

(declare (unit parse))

(require-extension srfi-1)
(require-extension srfi-9)
(require-extension posix)

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
(define (close-parse-stream stm) (close-input-port (get-in-port stm)))

;; trying a read-byte instead of read-char read-char seems to do weird stuff with extended ascii.

(define (get-next-char in) 
  (set-read-stack! in (cons (+ 1 (car (get-read-stack in))) (cdr (get-read-stack in))))
  (let ((b (read-char (get-in-port in))))
    (if (integer? b)
        (integer->char b)
        b)))

(define (push-stream-frame! in)
  (set-read-stack! in (cons (car (get-read-stack in)) (get-read-stack in))))

;(define (discard-stream-frame! in)
;  (set-read-stack! in (cdr (get-read-stack in)))
;  (close-input-port (get-in-port in))
;  (set-in-port! in (open-input-file (parse-stream-get-file in)))
;  ((rec-lambda (rec-call i)
;              (if (eq? (car (get-read-stack in)) i)
;                  #f
;                  (begin
;                    (read-char (get-in-port in))
;                    (rec-call (+ i 1))))) 0))

; PLT Supports file-position
; HUGE speed boost!

(define (discard-stream-frame! in)
  (set-read-stack! in (cdr (get-read-stack in)))
  (set-file-position! (get-in-port in) (car (get-read-stack in))))


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

(define (call-with-abort-parse-stack parser in)
  (push-stream-frame! in)
  (let ((out (parser in)))
    (discard-stream-frame! in)
    out))

;; primitive parsers

(define (parse-fail in) #f)
(define (parse-success in) #t)

(define (parse-char in) (get-next-char in))
(define (parse-a-char char) (lambda (in)
                              (let ((c (parse-char in)))
                                (if (eq? c char)
                                    c
                                    #f))))

(define (parse-eof in) (eof-object? (parse-char in)))

(define (parse-any-char chars in)
  (let* ((r (parse-char in)))
    (find (lambda (c) (eq? c r)) chars)))
    

(define (parse-whitespace in)
  (parse-any-char '(#\space #\tab) in))

(define (parse-newline in)
  (parse-any-char '(#\newline #\return) in))

(define (parse-str str)
  (lambda (in)
    (if (eq? (string-length str) 0)
        #t
        (if ((parse-a-char (string-ref str 0)) in)
            (if ((parse-str (substring str 1 (string-length str))) in)
                str
                #f)
            #f))))

(define (parse-num-char in)
  (parse-any-char (string->list "0123456789") in))

;; parser combinators

(define (recv-pass . expr) expr)

(define (parse-context parser)
  (lambda (in) (call-with-parse-stack parser in)))

(define (parse-or . parser)
  (lambda (in)
    (if (not (null? parser))
        (let ((out (call-with-parse-stack (car parser) in)))
          (if out
              out
              ((apply parse-or (cdr parser)) in)))
        #f)))

(define (parse-* recv parser)
  (letrec ((parser-loop (lambda (in)
                          (let ((out (call-with-parse-stack parser in)))
                            (if out
                                (cons out (parser-loop in))
                                '())))))
           (lambda (in) (let ((out (parser-loop in)))
                          (if (eq? out '())
                              #f
                              (apply recv out))))))

(define (parse-not parser)
  (lambda (in) (list->string
                ((rec-lambda (rec-call in)
                             (if (not (call-with-abort-parse-stack parser in))
                                 (cons (parse-char in) (rec-call in))
                                 '())) in))))
           

(define (parse-last . parser)
  (lambda (in)
    (let ((out ((car parser) in)))
      (if out
          (if (null? (cdr parser))
              out
              ((parse-last (cdr parser)) in))
          #f))))

(define (parse-sequence recv . parser)
  (lambda (in)
    (let ((ret
           (((rec-lambda (rec-call parser)
                         (lambda (in)
                           (if (null? parser)
                               '()
                               (cons
                                ((car parser) in)
                                ((rec-call (cdr parser)) in))))) parser) in)))
      (if (any not ret)
          #f
          (apply recv ret)))))

;; Additional parsers

(define parse-num
  (parse-sequence (lambda expr
                    (string->number (list->string (append (car expr) (list (cadr expr)) (caddr expr)))))
                  (parse-* recv-pass parse-num-char)
                  (parse-a-char #\.)
                  (parse-* recv-pass parse-num-char)))

(define (parse-int in)
  (let ((ret ((parse-* recv-pass parse-num-char) in)))
    (if ret
        (string->number (list->string ret))
        #f)))

(define parse-eat-whitespace (parse-* (lambda expr 'whitespace) parse-whitespace))
(define parse-eat-space-line (parse-* (lambda expr 'whitespace) (parse-or parse-whitespace parse-newline)))
(define parse-space parse-eat-whitespace)

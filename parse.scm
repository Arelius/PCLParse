#lang scheme
;; utility macros

(require srfi/1)
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
(define (close-parse-stream stm) (close-input-port (get-in-port stm)))

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
  (parse-any-char '(#\space #\tab #\newline #\return) in))

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
  (parse-sequence (lambda (. expr) 
                    (string->number (list->string (append (car expr) (list (cadr expr)) (caddr expr)))))
                  (parse-* recv-pass parse-num-char)
                  (parse-a-char #\.)
                  (parse-* recv-pass parse-num-char)))

(define (parse-int in)
  (let ((ret ((parse-* recv-pass parse-num-char) in)))
    (if ret
        (string->number (list->string ret))
        #f)))

(define parse-eat-whitespace (parse-* (lambda ( . expr) 'whitespace) parse-whitespace))
(define parse-space parse-eat-whitespace)

;; pcl parser

(define-record-type line
  (make-line type x y len)
  line?
  (type get-line-type set-line-type!)
  (x get-line-x set-line-x!)
  (y get-line-y set-line-y!)
  (len get-line-len set-line-len!))

(define-record-type line-width
  (make-line-width width)
  line-width?
  (width get-line-width))

(define (parse-a-line typ str)
  (parse-sequence
   (lambda (str spc1 x spc2 y spc3 len) (make-line typ x y len))
   (parse-str str)
   parse-space
   parse-num
   parse-space
   parse-num
   parse-space
   parse-num))

(define parse-line (parse-or
                    (parse-a-line 'hline "hlin")
                    (parse-a-line 'vline "vlin")))

(define parse-lwid (parse-sequence
                    (lambda (lwid spc width) (make-line-width width))
                    (parse-str "lwid")
                    parse-space
                    parse-int))

(define parse-command (parse-sequence
                       (lambda (expr) (car expr))
                       (parse-or
                        parse-line
                        parse-lwid)
                       (parse-or
                        parse-space
                        parse-eof)))
  ;                     parse-box
;                       parse-font
;                       parse-text))

(define parse-pcl (parse-* recv-pass parse-command))

(define test-in (open-parse-stream "test.pcl"))
(call-with-parse-stack parse-pcl test-in)
(close-parse-stream test-in)
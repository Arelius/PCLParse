;; utility macros

(require-extension riaxpander)

(define-syntax while
  (syntax-rules ()
    ((while cond) (letrec ((loop (lambda () (if cond (loop) #f)))) (loop)))
     ((while cond body ...) (letrec ((loop (lambda () (if cond (begin body ... (loop)) #f)))) (loop)))))

(define-syntax rec-lambda
  (er-macro-transformer
   (lambda (e r c)
     `(,(r 'letrec) ((rec-call (,(r 'lambda) ,(cadr e) ,@(cddr e))))
       rec-call))))

;; stream handling

(define-record-type parse-stream
  (make-parse-stream file in-port read-stack)
  parse-stream?
  (file get-file)
  (in-port get-in-port set-in-port!)
  (read-stack get-read-stack set-read-stack!))

(define (open-parse-stream file) (make-parse-stream file (open-input-file file) '(0)))

(define (get-next-char in) (set-car! (get-read-stack in) (+ 1 (car (get-read-stack in))))
  (read-char (get-in-port in)))

(define (push-stream-frame! in)
  (set-read-stack! in (cons (car (get-read-stack in)) (get-read-stack in))))

(define (discard-stream-frame! in)
  (set-read-stack! in (cdr (get-read-stack in)))
  (close-input-port (get-in-port in))
  (set-in-port! in (open-input-file))
  (rec-lambda (i)
              (if (eq? (car (get-read-stack in)) i)
                  #f
                  (begin
                    (read-char (get-in-port in))
                    (rec-call (+ i 1))))))


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
      #t
      (let ((out (parse-a-char (car chars) in)))
        (if out
            (parse-any-char (cdr chars) in)
            #f))))

(define (parse-whitespace in)
  (parse-any-char '(#\space #\tab #\newline)))

(define (parse-str str in)
  (if (eq? (string-length str) 0)
      #t
      (if (parse-a-char (string-ref str 0))
          (if (parse-str (substring str 1 (string-length str)) in)
              str
              #f))))

(define (parse-num-char in)
  (parse-any-char '(0 1 2 3 4 5 6 7 8 9) in))

;; parser combinators

(define (parse-context parser)
  (lambda (in) (call-with-parse-stack-frame parser in)))

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
                          (let ((out (parse-context (parser in))))
                            (if out
                                (cons out (parser-loop in))
                                '())))))))

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
    (let ((out ((car parser) in))
          (ret (if (and out (cdr parser))
               ((parse-sequence (cdr parser)) in)
               #f))))
    (if (cdr parser)
        (if ret
            (cons out ret)
            #f)
        (cons out '()))))

;; Additional parsers

(define parse-num )

(define parse-eat-whitespace (parse-* parse-whitespace))

;; pcl parser

(define-record-type line
  (make-line type x y len)
  line?
  (type get-line-type set-line-type!)
  (x get-line-x set-line-x!)
  (y get-line-y set-line-y!)
  (len get-line-len set-line-len!))

(define parse-space-num in (parse-last parse-eat-whitespace parse-num))

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
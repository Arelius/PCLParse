(require-extension srfi-9)

(declare (unit pcl_parser)
         (uses parse pcl_types))

;; pcl parser

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

(define parse-font (parse-sequence
                    (lambda (font spc str size mods) (make-font str size mods))
                    (parse-str "font")
                    parse-space
                    parse-alpha-string
                    parse-int
                    (parse-?
                     parse-alpha-string
                     "")))

(define parse-length-text
  (parse-sequence
   (lambda (text spc1 x spc2 y spc3 l spc4 str) (make-text x y str l))
   (parse-str "textl")
   parse-whitespace
   parse-num
   parse-whitespace
   parse-num
   parse-whitespace
   parse-num
   parse-whitespace
   (parse-not (parse-or
               parse-eof
               (parse-a-char #\return)
               (parse-a-char #\newline)))))

(define parse-text-reg
  (parse-sequence
   (lambda (text spc1 x spc2 y spc3 str) (make-text x y str #f))
   (parse-str "text")
   parse-space
   parse-num
   parse-space
   parse-num
   parse-space
   (parse-not (parse-or
               parse-eof
               (parse-a-char #\return)
               (parse-a-char #\newline)))))

(define parse-text
  (parse-or
   parse-text-reg
   parse-length-text))

(define parse-box (parse-sequence
                   (lambda (box spc1 x1 spc2 y1 spc3 x2 spc4 y2) (make-box x1 y1 x2 y2))
                   (parse-str "box")
                   parse-space
                   parse-num
                   parse-space
                   parse-num
                   parse-space
                   parse-num
                   parse-space
                   parse-num))

(define parse-shade (parse-sequence
                     (lambda (shade spc1 x1 spc2 y1 spc3 x2 spc4 y2 spc5 depth) (make-shade x1 y1 x2 y2 depth))
                     (parse-str "shade")
                     parse-space
                     parse-num
                     parse-space
                     parse-num
                     parse-space
                     parse-num
                     parse-space
                     parse-num
                     parse-space
                     parse-int))
                      

(define parse-command (parse-sequence
                       (lambda (front space) front)
                       (parse-or
                        parse-line
                        parse-box
                        parse-shade
                        parse-lwid
                        parse-font
                        parse-text)
                       (parse-or
                        parse-eat-space-line
                        parse-eof)))


(define parse-pcl (parse-* recv-pass parse-command))

(define (parse-pcl-file file)
  (let* ((pcl-in (open-parse-stream file))
         (pcl-stream (call-with-parse-stack parse-pcl pcl-in)))
    (close-parse-stream pcl-in)
    pcl-stream))

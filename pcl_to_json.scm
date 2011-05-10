(require-extension json)
(require-extension srfi-13)

(declare (uses parse pcl_parser pcl_types))

(define pcl-in (list-ref (command-line-arguments) 0))
(define pcl-out (list-ref (command-line-arguments) 1))

(define pcl-stream (parse-pcl-file pcl-in))

(define (pcl-font->family font)
  (cond
   ((equal? font "STMS")
    "Times New Roman")
   ((equal? font "SARIAL")
    "Arial")
   (default
     (display "Unrecogonized font.")
     "Times New Roman")))

(define (pcl-font-modifiers->font-style style)
  (string-join
   (list
    (if (string-any #\B style)
        "Bold"
        "")
    (if (string-any #\I style)
        "Italic"
        ""))
   " " 'infix))

(define (pcl-type->json-item pcl-type)
  (list->vector
   (cond
    ((line? pcl-type)
     `((element . ,(get-line-type pcl-type))
       (x . ,(get-line-x pcl-type))
       (y . ,(get-line-y pcl-type))
       (length . ,(get-line-len pcl-type))))
    ((line-width? pcl-type)
     `((element . lwid)
       (width . ,(get-line-width pcl-type))))
    ((font? pcl-type)
     `((element . font)
       (family . ,(pcl-font->family (get-font-str pcl-type)))
       (size . ,(get-font-size pcl-type))
       (modifiers . ,(pcl-font-modifiers->font-style (get-font-mods pcl-type)))))
    ((text? pcl-type)
     `((element . text)
       (x . ,(text-get-x pcl-type))
       (y . ,(text-get-y pcl-type))
       (text . ,(text-get-str pcl-type))))
    ((box? pcl-type)
     `((element . box)
       (x . ,(box-get-x1 pcl-type))
       (y . ,(box-get-y1 pcl-type))
       (x2 . ,(box-get-x2 pcl-type))
       (y2 . ,(box-get-y2 pcl-type))))
    ((shade? pcl-type)
     `((element . shade)
       (x . ,(shade-get-x1 pcl-type))
       (y . ,(shade-get-y1 pcl-type))
       (x2 . ,(shade-get-x2 pcl-type))
       (y2 . ,(shade-get-y2 pcl-type))
       (depth . ,(shade-get-depth pcl-type)))))))

(define (pcl-list->json-list pcl-list)
  (if (not (null? pcl-list))
      (cons
       (pcl-type->json-item (car pcl-list))
       (pcl-list->json-list (cdr pcl-list)))
      '()))

(define (pcl->json-list pcl-list)
  (list->vector
   `((pcl .
          ,(pcl-list->json-list pcl-list)))))

(define json-pcl-stream (pcl->json-list pcl-stream))

(call-with-output-file pcl-out (lambda (port) (json-write json-pcl-stream port)))
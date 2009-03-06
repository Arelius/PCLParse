(declare (unit pcl_writer)
         (uses pcl_types))

(define (pcl-type->primitive-list pcl-type)
  (cond
    ((line? pcl-type)
     (list
      (if (eq? (get-line-type pcl-type) 'hline) "hlin" "vlin")
      (get-line-x pcl-type)
      (get-line-y pcl-type)
      (get-line-len pcl-type)))
    ((line-width? pcl-type)
     (list
      "lwid"
      (get-line-width pcl-type)))
    ((font? pcl-type)
     (list
      "font"
      (get-font-str pcl-type)))
    ((text? pcl-type)
     (list
      "text"
      (text-get-x pcl-type)
      (text-get-y pcl-type)
      (text-get-str pcl-type)))
    ((box? pcl-type)
     (list
      "box"
      (box-get-x1 pcl-type)
      (box-get-y1 pcl-type)
      (box-get-x2 pcl-type)
      (box-get-y2 pcl-type)))
    ((shade? pcl-type)
     (list
      "shade"
      (shade-get-x1 pcl-type)
      (shade-get-y1 pcl-type)
      (shade-get-x2 pcl-type)
      (shade-get-y2 pcl-type)
      (shade-get-depth pcl-type)))))

(define (pcl-list->primitive-list-list pcl-list)
  (if (not (null? pcl-list))
      (cons
       (pcl-type->primitive-list (car pcl-list))
       (pcl-list->primitive-list-list (cdr pcl-list)))
      '()))

(define (write-primitive-list lst file)
  (for-each
   (lambda (primv-lst) (display primv-lst file))
   lst))

;; Post primitive list transformations.
(define (intersperse seperator xs)
  (cond
    [(null? xs) '()]
    [(null? (cdr xs)) xs]
    [else (cons (car xs)
                (cons seperator
                      (intersperse seperator (cdr xs))))]))

(define (primv-list-tab-commands lst)
  (if (null? lst)
      '()
      (cons
       (if (list (car lst))
           (intersperse #\tab (car lst))
           (car lst))
       (primv-list-tab-commands (cdr lst)))))

(define (primv-list-add-newlines lst)
  (if (not (null? lst))
      (cons (car lst)
            (cons #\return (cons #\newline
                                 (primv-list-add-newlines (cdr lst)))))
      '()))

(define (primv-list-flatten lst)
  (if (null? lst)
      '()
      (if (list? (car lst))
          (append (car lst) (primv-list-flatten (cdr lst)))
          (cons (car lst) (primv-list-flatten (cdr lst))))))

(define (primv-list-truncate-numbers lst)
  (if (null? lst)
      '()
      (cons
       (if (number? (car lst))
           (number->string (/ (round (* (car lst) 1000)) 1000))
           (car lst))
       (primv-list-truncate-numbers (cdr lst)))))
            

(define primitive-list-transforms (list primv-list-truncate-numbers primv-list-flatten primv-list-add-newlines primv-list-tab-commands))

(define (write-pcl pcl-list file)
  (write-primitive-list
   ((apply compose primitive-list-transforms)
    (pcl-list->primitive-list-list pcl-list))
   file))
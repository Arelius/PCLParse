(declare (unit pcl_types))
(require-extension srfi-9)

;; pcl types

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

(define-record-type font
  (make-font font-str pt-size mods)
  font?
  (font-str get-font-str)
  (pt-size get-font-size)
  (mods get-font-mods))

(define-record-type text
  (make-text x y str)
  text?
  (x text-get-x)
  (y text-get-y)
  (str text-get-str))

(define-record-type box
  (make-box x1 y1 x2 y2)
  box?
  (x1 box-get-x1)
  (y1 box-get-y1)
  (x2 box-get-x2)
  (y2 box-get-y2))

(define-record-type shade
  (make-shade x1 y1 x2 y2 d)
  shade?
  (x1 shade-get-x1)
  (y1 shade-get-y1)
  (x2 shade-get-x2)
  (y2 shade-get-y2)
  (d shade-get-depth))
   

(define (pcl-coord-obj? x)
  (or (line? x) (box? x) (shade? x) (text? x)))

(define (pcl-obj-x x)
  (cond
    ((line? x)
     (get-line-x x))
    ((box? x)
     (box-get-x1 x))
    ((shade? x)
     (shade-get-x1 x))
    ((text? x)
     (text-get-x x))))

(define (pcl-obj-y x)
  (cond
    ((line? x)
     (get-line-y x))
    ((box? x)
     (box-get-y1 x))
    ((shade? x)
     (shade-get-y1 x))
    ((text? x)
     (text-get-y x))))

(define (pcl<? l r)
  (cond
    ((and (line-width? l) (line-width? r))
     (> (get-line-width l) (get-line-width r)))
    ((and (font? l) (font? r))
     (if (= (get-font-size l) (get-font-size r))
         (< (get-font-size l) (get-font-size r))
         (string<? (get-font-str l) (get-font-str r))))
    ((and (pcl-coord-obj? l) (pcl-coord-obj? r))
     (let ((ly (pcl-obj-y l))
           (ry (pcl-obj-y r)))
       (if (= ly ry)
           (< (pcl-obj-x l) (pcl-obj-x r))
           (< ly ry))))
    (else (error (string-append "Comparing two incompatable pcl types:" (->string l) " " (->string r) ".")))))

(define (pcl=? l r)
  (cond
    ((and (line-width? l) (line-width? r))
     (eq? (get-line-width l) (get-line-width r)))
    ((and (font? l) (font? r))
     (and (equal? (get-font-str l) (get-font-str r))
          (equal? (get-font-size l) (get-font-size r))
          (equal? (get-font-mods l) (get-font-mods r))))
    (else (error (string-append "Comparing two incompatable pcl types:" (->string l) " " (->string r) ".")))))

(define (pcl-discard? l r)
  (and
   (line? l)
   (line? r)
   (= (get-line-x l)
      (get-line-x r))
   (= (get-line-y l)
      (get-line-y r))
   (eq? (get-line-type l)
        (get-line-type r))
   (>= (get-line-len l)
      (get-line-len r))))
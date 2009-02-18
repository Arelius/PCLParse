#lang scheme

(provide (all-defined-out))
(require srfi/9)

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
  (make-font font-str)
  font?
  (font-str get-font-str))

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

(define (pcl-coord-obj? x)
  (or (line? x) (box? x) (text? x)))

(define (pcl-obj-x x)
  (cond
    ((line? x)
     (get-line-x x))
    ((box? x)
     (box-get-x1 x))
    ((text? x)
     (text-get-x x))))

(define (pcl-obj-y x)
  (cond
    ((line? x)
     (get-line-y x))
    ((box? x)
     (box-get-y1 x))
    ((text? x)
     (text-get-y x))))

(define (pcl<? l r)
  (cond
    ((and (line-width? l) (line-width? r))
     (> (get-line-width l) (get-line-width r)))
    ((and (font? l) (font? r))
     (string<? (get-font-str l) (get-font-str r)))
    ((and (pcl-coord-obj? l) (pcl-coord-obj? r))
     (let ([lx (pcl-obj-x l)]
           [rx (pcl-obj-x r)])
       (if (= lx rx)
           (< (pcl-obj-y l) (pcl-obj-y r))
           (< lx rx))))
    (else (error (string-append "Comparing two incompatable pcl types:" l " " r ".")))))

(define (pcl=? l r)
  (cond
    ((and (line-width? l) (line-width? r))
     (eq? (get-line-width l) (get-line-width r)))
    ((and (font? l) (font? r))
     (equal? (get-font-str l) (get-font-str r)))
    (else (error (string-append "Comparing two incompatable pcl types:" l " " r ".")))))
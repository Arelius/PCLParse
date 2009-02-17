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

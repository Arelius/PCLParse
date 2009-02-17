#lang scheme/gui

(require "pcl_parser.scm")
(require "pcl_types.scm")

(define pcl-stream test-stream)

(define ed-frame (new frame%
                      [label "PCL Edit"]
                      [width 400]
                      [height 600]))

(define ed-text-canvas (new editor-canvas% [parent ed-frame]))
(define ed-text (new text%))
(send ed-text-canvas set-editor ed-text)

(define ed-board-canvas (new editor-canvas% [parent ed-frame]))
(define ed-board (new pasteboard%))
(send ed-board-canvas set-editor ed-board)

(send ed-frame show #t)

(define hline-snip%
  (class snip%
    (init-field length)
    (init-field orient)
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (if w (set-box! w (if (eq? orient 'hori) length 1)) '())
      (if h (set-box! h (if (eq? orient 'hori) 1 length)) '())
      (if descent (set-box! descent 0) '())
      (if space (set-box! space 0) '())
      (if lspace (set-box! lspace 0) '())
      (if rspace (set-box! rspace 0) '()))
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (if (eq? orient 'hori)
          (send dc draw-line x y (+ x length) y)
          (send dc draw-line x y x (+ y length))))
    (define/override (resize w h)
      (if (eq? orient 'hori)
          (set! length w)
          (set! length h))
      (send (send this get-admin) resized this #t)
      #t)
    (super-new)))

(define (coord-pcl->canvas i) (* i 30))

(define (add-hline x y len)
  (let ((lin (make-object hline-snip% (coord-pcl->canvas len) 'hori)))
    (send lin set-snipclass (new snip-class%))
    (send ed-board insert lin (coord-pcl->canvas x) (coord-pcl->canvas y))))

(define (add-vline x y len)
  (let ((lin (make-object hline-snip% (coord-pcl->canvas len) 'vert)))
    (send lin set-snipclass (new snip-class%))
    (send ed-board insert lin (coord-pcl->canvas x) (coord-pcl->canvas y))))

(define (add-text x y str)
  (send ed-board insert (make-object string-snip% str) (coord-pcl->canvas x) (coord-pcl->canvas y)))

(define (add-snips-from-pcl-stream stm)
  (cond
    ((and (line? (car stm)) (eq? (get-line-type (car stm)) 'hline) (add-hline (get-line-x (car stm)) (get-line-y (car stm)) (get-line-len (car stm)))))
    ((and (line? (car stm)) (eq? (get-line-type (car stm)) 'vline) (add-vline (get-line-x (car stm)) (get-line-y (car stm)) (get-line-len (car stm)))))
    ((text? (car stm)) (add-text (text-get-x (car stm)) (text-get-y (car stm)) (text-get-str (car stm)))))
  (if (not (null? (cdr stm)))
      (add-snips-from-pcl-stream (cdr stm))
      '()))

(add-snips-from-pcl-stream pcl-stream)
     
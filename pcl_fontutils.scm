(declare (unit pcl_fontutils)
         (uses pcl_types))

(require-extension freetype)

(define ft-lib (ft-init-freetype))

(define ft-dpi 300)

;;(define selected-font-face (ft-new-face ft-lib "/Library/Fonts/Times New Roman.ttf"))
(define selected-font-face (ft-new-face ft-lib "/Library/Fonts/Arial.ttf"))
(ft-set-char-size selected-font-face 0 (* 10 64) ft-dpi ft-dpi)

(define (get-ft-font-face font)
  (let ((face (ft-new-face
               ft-lib
               (cond
                 ((equal? (get-font-str font) "STMS")
                  "/Library/Fonts/Times New Roman.ttf")
                 ((equal? (get-font-str font) "SARIAL")
                  "/Library/Fonts/Arial.ttf")))))
    (ft-set-char-size face 0 (* (get-font-size font) 64) ft-dpi ft-dpi)
    face))

(define (pp->pixels pp) (arithmetic-shift pp -6))
(define (pixels->inches px) (/ px ft-dpi))
(define (inches->cm in) (* 2.53 in))

(define pp->cm (compose
                inches->cm pixels->inches pp->pixels))

(define (iterate-glyph func string face)
	(for-each
	 (lambda (c)
	   (ft-load-char face (char->integer c) FT_LOAD_DEFAULT)
	   (ft-render-glyph (ft-face-glyph face) FT_RENDER_MODE_NORMAL)
	   (let ((glyph (ft-face-glyph face)))
		 (func glyph)))
	 (string->list string)))

(define (ft-get-string-pp-width string face)
  (let ((string-pp-width 0))
    (iterate-glyph
     (lambda (glyph)
       (set! string-pp-width
             (+ string-pp-width
                (ft-glyph-metrics-hori-advance
                 (ft-glyph-slot-metrics glyph)))))
     string
     face)
    string-pp-width))

(define (ft-get-string-cm-width string face)
  (pp->cm (ft-get-string-pp-width string face)))

(define font-close-threshold 0.05)

(define (<> c h l)
  (and
   (< c h)
   (> c l)))

(define (pcl-text-combine? face l r)
  (and (= (pcl-obj-y l) (pcl-obj-y r))
       (<>
        (- (pcl-obj-x r)
           (pcl-obj-x l)
           (ft-get-string-cm-width (text-get-str l) face))
        font-close-threshold
        (- font-close-threshold))))

(define (pcl-text-do-combine l r)
  (make-text
   (text-get-x l)
   (text-get-y l)
   (string-append
    (text-get-str l)
    (text-get-str r))))

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
	(letrec
        ((iterate
          (lambda (lst previous-glyph)
            (if (null? lst)
                '()
                (begin
                  (ft-load-char face (char->integer (car lst)) FT_LOAD_DEFAULT)
                  (ft-render-glyph (ft-face-glyph face) FT_RENDER_MODE_NORMAL)
                  (let ((glyph (ft-face-glyph face)))
                    (cons
                     (func glyph)
                     (iterate (cdr lst) glyph))))))))
	 (iterate
      (if (string? string)
          (string->list string)
          string) #f)))

(define (ft-get-char-pair-kerning-pp-width lchar rchar face)
  (let* ((Vect (make-ft-vector))
         (err (ft-get-kerning
               face
               (ft-get-char-index face (char->integer lchar))
               (ft-get-char-index face (char->integer rchar))
               FT_KERNING_DEFAULT
               Vect)))
    (if (eq? 0 err)
        (ft-vector-x Vect)
        0)))

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


(define (ft-get-strings-kerning-pp-width face . rest)
  (letrec ((char-list (apply
                       append
                       (map
                        string->list
                        rest)))
           (get-kerning
            (lambda (char rest)
              (+
               (ft-get-char-pair-kerning-pp-width char rest face)
               (if (null? rest)
                   0
                   (get-kerning (car rest) (cdr rest)))))))
    (get-kerning (car char-list) (cdr char-list))))

(define (ft-get-string-cm-width string face)
  (pp->cm (ft-get-string-pp-width string face)))

(define font-close-threshold 0.04)

(define (<> c h l)
  (and
   (<= c h)
   (>= c l)))

(define (duplicate x i)
  (if (= i 0)
      '()
      (cons x (duplicate x (- i 1)))))

(define (pcl-text-combine? face l r extra)
  (and (= (pcl-obj-y l) (pcl-obj-y r))
       (<>
        (- (pcl-obj-x r)
           (pcl-obj-x l)
           (ft-get-string-cm-width (text-get-str l) face)
           (ft-get-strings-kerning-pp-width face
                                            (text-get-str l)
                                            (string-take (text-get-str r) 1))
           extra)
        font-close-threshold
        (- font-close-threshold))))

(define (pcl-text-do-combine l r spaces-count)
  (make-text
   (text-get-x l)
   (text-get-y l)
   (apply
    string-append
    (append
     (list (text-get-str l))
     (duplicate " " spaces-count)
     (list (text-get-str r))))))

(define (pcl-text-try-combine face txt rest)
  (cond ((pcl-text-combine? face txt (car rest) 0)
         (cons
          (pcl-text-do-combine txt (car rest) 0)
          (cdr rest)))
        ((pcl-text-combine? face txt (car rest) (ft-get-string-cm-width " " face))
         (cons
          (pcl-text-do-combine txt (car rest) 1)
          (cdr rest)))
        (else (cons txt rest))))

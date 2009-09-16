(declare (unit pcl_transform)
         (uses pcl_types))

(require-extension srfi-1)
;;(require-extension srfi-32)


(define (true? x) x)

(define nest-tree (list (list line-width? line? box? shade?) (list font? text?)))

(define (extract-state-nest state lst)
  (if (null? lst)
      '()
      (if ((car state) (car lst))
          (cons
           (cons
            (car lst)
            (letrec ([extract-state-children
                      (lambda (state lst)
                        (if (or (null? lst) ((car state) (car lst)))
                            '()
                            (let ([match (find true?
                                               (map 
                                                (lambda (f)
                                                  (if (f (car lst)) (car lst) #f))
                                                (cdr state)))])
                              (if match
                                  (cons match (extract-state-children state (cdr lst)))
                                  (extract-state-children state (cdr lst))))))])
              (extract-state-children state (cdr lst))))
            (extract-state-nest state (cdr lst)))
          (extract-state-nest state (cdr lst)))))
           
                             

(define (nest-pcl-list lst)
  (map (lambda (state) (extract-state-nest state lst)) nest-tree))

(define (unnest-pcl-list lst)
  (concatenate (concatenate lst)))

;; List transformers

(define (transform-remove-empty-text lst)
  (remove (lambda (elm)
            (and (text? elm) (eq? (string-length (text-get-str elm)) 0)))
          lst))

(define transform-func-list (list transform-remove-empty-text))

;; Nested list transformers.

(define (nest-transform-sort lst)
  (map
   (lambda (lst)
     (sort lst (lambda (l r) (pcl<? (car l) (car r)))))
   lst))

(define (nest-transform-merge-similar lst)
  (map
   (letrec
       ([merge-s (lambda (mlst)
                   (if (null? mlst)
                       '()
                       (let ([l (car mlst)]
                             [r (merge-s (cdr mlst))])
                         (if (and (not (null? r)) (pcl=? (car l) (caar r)))
                             (cons (append l (cdar r)) (cdr r))
                             (cons l r)))))])
      merge-s)
   lst))

;; This actually requires lines to be sorted longest to shortest, which isn't currentlly done.
(define (nest-transform-merge-overlapping! lst)
  (map
   (lambda (lst)
     (map
      (lambda (lst)
        (letrec
            ((merge-o
              (lambda (l st)
                (if (null? (cdr st))
                    (if (null? (cdr l))
                        '()
                        (merge-o (cdr l) (cdr l)))
                    (if (pcl-discard? (car l) (cadr st))
                        (begin
                          (set-cdr! st (cddr st))
                          (merge-o l st))
                        (merge-o l (cdr st)))))))
          (merge-o lst lst)
          lst)
        )
      lst))
   lst))

(define (nest-transform-remove-empty lst)
  (map
   (lambda (lst)
     (remove (lambda (x) (null? (cdr x))) lst))
   lst))

(define (nest-transform-sub-sort lst)
  (map
   (lambda (lst)
     (map
      (lambda (lst)
        (cons
         (car lst)
         (sort (cdr lst) pcl<?)))
      lst))
   lst))

(define nest-transform-func-list (list nest-transform-merge-overlapping! nest-transform-sub-sort nest-transform-remove-empty nest-transform-merge-similar nest-transform-sort))


(define (transform-pcl-list lst)
  (unnest-pcl-list
   ((apply compose nest-transform-func-list)
    (nest-pcl-list
     ((apply compose transform-func-list) lst)))))
      
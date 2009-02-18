#lang scheme

(require "pcl_types.scm")
(require srfi/1)

(provide (all-defined-out))

(define (true? x) x)

(define nest-tree (list (list line-width? line? box?) (list font? text?)))

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

(define transform-func-list (list (lambda (x) x)))
(define nest-transform-func-list (list (lambda (x) x)))


(define (transform-pcl-list lst)
  (unnest-pcl-list
   ((apply compose nest-transform-func-list)
    (nest-pcl-list
     ((apply compose transform-func-list) lst)))))
      
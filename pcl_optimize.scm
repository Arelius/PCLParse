#lang scheme
(require "parse.scm")
(require "pcl_parser.scm")
(require "pcl_types.scm")
(require "pcl_writer.scm")
(require "pcl_transform.scm")

(define pcl-in (vector-ref (current-command-line-arguments) 0))
(define pcl-out (vector-ref (current-command-line-arguments) 1))
;(define pcl-in "I134A.pcl")
;(define pcl-out "TI134A.pcl")

(define pcl-stream (parse-pcl-file pcl-in))

(define transformed-pcl-stream (transform-pcl-list pcl-stream))

;(write-pcl transformed-pcl-stream (current-output-port))
(call-with-output-file pcl-out (lambda (port) (write-pcl transformed-pcl-stream port)))
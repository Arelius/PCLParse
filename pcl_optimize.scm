#lang scheme
(require "parse.scm")
(require "pcl_parser.scm")
(require "pcl_types.scm")
(require "pcl_writer.scm")

(define pcl-stream (parse-pcl-file "test.pcl"))

(pcl-list->primitive-list-list pcl-stream)

(define pcl-transform-list (list (lambda (x) x)))

(write-pcl pcl-stream (current-output-port))
;(call-with-output-file "testout.pcl" (lambda (port) (write-pcl pcl-stream port)))
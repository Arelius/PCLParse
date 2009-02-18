#lang scheme
(require "parse.scm")
(require "pcl_parser.scm")
(require "pcl_types.scm")
(require "pcl_writer.scm")
(require "pcl_transform.scm")

(define pcl-stream (parse-pcl-file "test.pcl"))

(define transformed-pcl-stream (transform-pcl-list pcl-stream))

(write-pcl transformed-pcl-stream (current-output-port))
;(call-with-output-file "testout.pcl" (lambda (port) (write-pcl transformed-pcl-stream port)))
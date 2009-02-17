#lang scheme
(require "parse.scm")
(require "pcl_parser.scm")
(require "pcl_types.scm")

(define pcl-stream (parse-pcl-file "test.pcl"))
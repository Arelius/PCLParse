(declare (uses parse pcl_parser pcl_types pcl_writer pcl_transform))

(define pcl-in (list-ref (command-line-arguments) 0))
(define pcl-out (list-ref (command-line-arguments) 1))
;(define pcl-in "I134A.pcl")
;(define pcl-out "TI134A.pcl")

(define pcl-stream (parse-pcl-file pcl-in))

(define transformed-pcl-stream (transform-pcl-list pcl-stream))

;(write-pcl transformed-pcl-stream (current-output-port))
(call-with-output-file pcl-out (lambda (port) (write-pcl transformed-pcl-stream port)))
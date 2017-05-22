#lang racket

(require "simulator6.rkt")
(require rackunit)


(check-equal? (try-flow-blocked-lane (list (make-car 1 (list 0 0) (list 0 1) 2 #f #f)
                                           (make-car 2 (list 0 0) (list 0 1) 2 #f #f)
                                           #f
                                           (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                                           (make-car 4 (list 0 0) (list 0 1) 2 #f #f))
                                     #t)
              (list #f
                    (make-car 1 (list 0 0) (list 0 1) 2 #f #f)
                    (make-car 2 (list 0 0) (list 0 1) 2 #f #f)
                    #f
                    (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                    ))

(check-equal? (try-flow-blocked-lane (list (make-car 1 (list 0 0) (list 0 1) 2 #f #f)
                                           (make-car 2 (list 0 0) (list 0 1) 2 #f #f)
                                           #f
                                           (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                                           (make-car 4 (list 0 0) (list 0 1) 2 #f #f))
                                     #f)
              (list #f
                    (make-car 1 (list 0 0) (list 0 1) 2 #f #f)
                    (make-car 2 (list 0 0) (list 0 1) 2 #f #f)
                    (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                    (make-car 4 (list 0 0) (list 0 1) 2 #f #f)
                    ))


(check-equal? (try-flow-blocked-lane (list #f
                                           #f
                                           #f
                                           (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                                           (make-car 4 (list 0 0) (list 0 1) 2 #f #f))
                                     #t)
              (list
               #f
               #f
               #f
               #f
               (make-car 3 (list 0 0) (list 0 1) 2 #f #f)
                    ))
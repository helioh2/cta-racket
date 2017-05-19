#lang racket
(define (try-flow-blocked-lane v normal-flow?)
  (local [
          (define (try-flow-blocked-lane-aux v i)
            (cond [(< i 1) v]
                  [else
                   (try-flow-blocked-lane-aux
                    (if (or normal-flow? (false? (vector-ref v i)))
                                                 
                        (for ([j (in-range (sub1 i) -1 -1)]                                
                              #:when (not (false? (vector-ref v j))))
                          (let ([car (vector-ref v j)])
                            (begin
                              (log-move-car (car-id car) #f)
                              
                              (vector-append (vector #f)
                                             (vector-take v j)
                                             (vector (make-car
                                                      (car-id car)
                                                      (car-block-id car)
                                                      (car-dir car)
                                                      (car-total-time car)
                                                      #f
                                                      (car-next-block-id car)
                                                      #f))
                                             (vector-drop v (add1 i))))))
                          v)
                    
                    (if (or normal-flow? (false? (vector-ref v i)))
                        0
                        (sub1 i)))]))

          ]
    
    (let ([trying (try-flow-blocked-lane-aux v 4)])
      (begin
        (if (void? trying) (begin (display (string-append "virou void quando v = ")) (display v)) #f)
      (if (equal? trying v)
          (list->vector (stop-cars (vector->list v)))
          trying)))))
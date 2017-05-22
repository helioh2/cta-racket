#lang racket

(require racket/stream)
(require racket/list)

;; CONSTANTES:

(define COUNT-STREETS 20)
(define C-STREET-HALF (/ COUNT-STREETS 2))
(define TURN-RATE 0.1)
(define BLOCK-LENGTH 5)
(define MAX-ITERS 1000)

(define SENSES (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0)))
(define SENSES_NAMES (list "RIGHT" "LEFT" "DOWN" "UP"))

(define DIRECTIONS (list (list 0 1) (list 1 0)))
(define DIRECTIONS_NAMES (list "HORIZONTAL" "VERTICAL"))

(define DIR_HOR 0)
(define DIR_VER 1)

(define RED 0)
(define GREEN 1)
(define YELLOW 2)

(define SEP "|")
(define LOGNAME "simulator.txt")
(define OUT (open-output-file LOGNAME #:exists 'truncate))


;; FUNCOES AUXILIARES:

(define (cria-pares item lista)
  (map (λ (item2) (list item item2)) lista))

(define (produto-cartesiano list1 list2)
  (cond [(empty? list1) empty]
        [else
         (append (cria-pares (first list1) list2)
                 (produto-cartesiano (rest list1) list2))]))


;; FUNÇÕES LOG

(define (log-init)
  (display "INIT|10|10|5|1|0|0|1|1\n" OUT))

(define (linebreak)
  (display "\n" OUT))

(define (log-new-car car-id street)
  (begin
    (display "CAR-NEW|" OUT)
    (display (string-append (number->string car-id) "|") OUT)
    (display (string-append (number->string (first (street-dir street))) "|") OUT)
    (display (if (= (first (street-dir street)) 0)
                 (number->string (street-id street))
                 (number->string (- (street-id street) 10))) OUT)
    (linebreak)))

(define (log-move-car car-id turn?)
  (begin
    (display "CAR-MOVE|" OUT)
    (display (string-append (number->string car-id) "|") OUT)
    (display (if turn? "1" "0") OUT)
    (linebreak)))

(define (log-exit-car car-id)
  (begin
    (display "CAR-EXIT|" OUT)
    (display (number->string car-id) OUT)
    (linebreak)))

(define (log-clock)
  (display "CLOCK|1\n" OUT))

(define (log-semaphore tfs timer int)
  (begin
    (display "SEMAPHORE|" OUT)
    (display (string-append (number->string (intersection-h-street int)) "|") OUT)
    (display (string-append (number->string (- (intersection-v-street int) 10)) "|") OUT)
    (display (cond [(and (= (first tfs) RED) (= (second tfs) RED) (> (first timer) (second timer)))
                    "0"]
                   [(and (= (first tfs) GREEN) (= (second tfs) RED))
                    "1"]
                   [(and (= (first tfs) YELLOW) (= (second tfs) RED))
                    "2"]
                   [(and (= (first tfs) RED) (= (second tfs) RED) (< (first timer) (second timer)))
                    "3"]
                   [(and (= (first tfs) RED) (= (second tfs) GREEN))
                    "4"]
                   [(and (= (first tfs) RED) (= (second tfs) YELLOW))
                    "5"]
                   )
             OUT)
    (linebreak)))


(define (log-info msg)
  (begin
    (display "LOG|I|" OUT)
    (display msg OUT)
    (linebreak)))
    
;; DEFINIÇÃO DE DADOS:

(define-struct car (id block-id dir total-time
                       in-intersection?
                       next-block-id) #:transparent)
;examples:
(define CAR1 (make-car 1 (list 0 0) (list 0 1) 2 #f #f))

(define-struct block (id lane dir sense) #:transparent)
;examples:
(define BLOCK1 (make-block (list 0 0)
                           (vector #f CAR1 #f #f #f)
                           (list 0 1)
                           (list 0 1)))

(define BLOCK2 (make-block (list 10 0) (make-vector 5 #f) (list 1 0) (list 1 0)))

(define (create-block street-id id dir sense)
  (make-block (list street-id id)
              (make-vector 5 #f)
              dir
              sense
              ))
              
(define (create-blocks street-id dir sense)
  (map (λ (x) (create-block street-id x dir sense))
       (stream->list (in-range (add1 C-STREET-HALF)))))

(define (next-intersection block ints)
  (if (or (= (first (block-sense block)) -1) (= (second (block-sense block)) -1))
      (previous-intersection block ints)
      (for/first ([int ints]
                  #:when (or (equal? (intersection-id int) (block-id block))
                             (equal? (intersection-id int) (list
                                                            (second (block-id block))
                                                            (- (first (block-id block)) C-STREET-HALF)))))
        int)
      ))

(define (previous-intersection block intersections)
  (for/first ([int intersections]
              #:when (or (equal? (intersection-id int) (list (first (block-id block))
                                                             (sub1 (second (block-id block)))))
                         (equal? (intersection-id int) (list
                                                        (sub1 (second (block-id block)))
                                                        (- (first (block-id block)) C-STREET-HALF)))))
    int)
  )
  

(define-struct street (id dir sense blocks entry-block exit-block) #:transparent)
;; Street é (make-street Integer+ Integer[0,1] PairInSENSES List[Block] Function Block Block)
;example
(define STR1 (make-street 0 0 (list 0 1) (list BLOCK1) BLOCK1 #f))
(define STR2 (make-street 1 1 (list 1 0) (list BLOCK2) BLOCK2 #f))


(define (create-street id dir)
  (let* (
         [calc-sense (λ (x) (if (= x 0) 0
                                (if (= (remainder id 2) 0) 1
                                    -1)))]
         [sense (list (calc-sense (first dir))
                      (calc-sense (second dir)))]
         [blocks (create-blocks id dir sense) ]
         )
    (make-street id
                 dir
                 sense
                 blocks
                 (if (or (= (first sense) 1) (= (second sense) 1))
                     (block-id (first blocks))
                     (block-id (last blocks)))
                 (if (or (= (first sense) -1) (= (second sense) -1))
                     (block-id (first blocks))
                     (block-id (last blocks))))
    )
  )

;; Street inicial é (create-street)

;;TrafficLight é um desses: RED, GREEN, YELLOW

(define-struct semaphore (traffic-lights timer) #:transparent)
;; Semaphore é (make-semaphore Pair(TrafficLight) Integer+)
;example:
(define SEM1 (make-semaphore (list RED RED) (list 0 1)))


(define-struct intersection (id h-street v-street entry-blocks exit-blocks crossing semaphore) #:transparent)
;; Intersection é (make-intersection '(Integer+,Integer+) Street Street Pair[Block] Pair[Block] Car|#false Semaphore)
;example
(define INT1 (make-intersection (list 0 0) STR1 STR2 (list BLOCK1 #f) (list BLOCK2 #f) #f SEM1))


;; Street -> Intersection
(define (create-intersection h-street v-street)
  (make-intersection (list (street-id h-street) (- (street-id v-street) (/ COUNT-STREETS 2)))
                     (street-id h-street)
                     (street-id v-street)
                     (list (block-id (list-ref (street-blocks h-street)
                                               (if (equal? (street-sense h-street) (list 0 1))
                                                   (- (street-id v-street) (/ COUNT-STREETS 2) )
                                                   (add1 (- (street-id v-street) (/ COUNT-STREETS 2) )))))                                         
                                     
                           (block-id (list-ref (street-blocks v-street)
                                               (if (equal? (street-sense v-street) (list 1 0))
                                                   (street-id h-street)
                                                   (add1 (street-id h-street)))
                                               )))   ;;REFATORAR!!!
                     (list (block-id (list-ref (street-blocks h-street)
                                               (if (equal? (street-sense h-street) (list 0 1))
                                                   (add1 (- (street-id v-street) (/ COUNT-STREETS 2) ))
                                                   (- (street-id v-street) (/ COUNT-STREETS 2) ))))
                           (block-id (list-ref (street-blocks v-street)
                                               (if (equal? (street-sense v-street) (list 1 0))
                                                   (add1 (street-id h-street))
                                                   (street-id h-street)
                                                   ))))  ;;REFATORAR!!!
                     #f
                     (make-semaphore (list RED RED) (list 47 1))))


;; List[Street] -> List[Intersection]
(define (create-intersections h-streets v-streets)
  (let* ([street-pairs (produto-cartesiano h-streets v-streets)]
         [intersections
          (map (λ (p) (create-intersection (first p) (second p)))
               street-pairs)
          ]
         )
     
   
    intersections
    ))

;; Intersection car -> Boolean
(define (intersection-closed int dir)
  (let* ([index-dir (first dir)]
         [sem (intersection-semaphore int)]
         [tf (list-ref (semaphore-traffic-lights sem)
                       index-dir)]
         [timer (list-ref (semaphore-timer sem)
                          index-dir)])
    (or (equal? tf RED)
        (and (equal? tf YELLOW)
             (<= timer 3)))))
    
  


(define-struct simulator (h-streets v-streets intersections
                                    count-iter car-id) #:transparent)
;example:
(define SIM1 (make-simulator (list STR1) (list STR2) (list INT1) 0 0))

(define (init-simulator)
  (let* (
         [h-streets
          (map (λ (i) (create-street i (first DIRECTIONS)))
               (stream->list (in-range 10)))
          ]
         [v-streets
          (map (λ (i) (create-street i (second DIRECTIONS)))
               (stream->list (in-range 10 20)))
          ]
         )
    (begin
      (log-init)
      (make-simulator
       h-streets
       v-streets
       (create-intersections h-streets v-streets) 0 0
       )
      )))

(define SIMT1 (init-simulator))

(require 2htdp/universe)

;; Big-Bang (ticks):

(define (main sim)
  (big-bang sim
            (on-tick tick)
            (to-draw draw)
            (stop-when stop?)
            ))



(require 2htdp/image)
(define (stop? sim)
  (> (simulator-count-iter sim) MAX-ITERS))

;; Intersection -> Intersection
(define (remove-car int)
  (make-intersection (intersection-id int)
                     (intersection-h-street int)
                     (intersection-v-street int)
                     (intersection-entry-blocks int)
                     (intersection-exit-blocks int)
                     #f
                     (intersection-semaphore int)))
   

(define (free-next-space? car streets)

  (local [
          (define (free-next-space-aux car blocks)
            (cond [(empty? blocks) #f]
                  [else
                   (or (and (equal? (block-id (first blocks)) (car-next-block-id car))
                            (false? (first (vector->list (block-lane (first blocks))))))
                       (free-next-space-aux car (rest blocks)))]))
          ]
  
  (cond [(empty? streets) #f]
        [else
         (or (free-next-space-aux car (street-blocks (first streets)))
             (free-next-space? car (rest streets)))])))

(define (leave-intersection car)
   (make-car
    (car-id car)
    (car-block-id car)
    (car-dir car)
    (car-total-time car)
    #f
    (car-next-block-id car)))

;; List[Intersection] -> (List[Intersection], List[car])
(define (try-move-crossings ints streets)
  (local [
          (define (try-move-crossings-aux ints ints-acc to-move-car)
            (cond [(empty? ints) (list ints-acc to-move-car)]
                  [else
                   (let ([car (intersection-crossing (first ints))])
                     (if (not (false? car))
                         (try-move-crossings-aux (rest ints)
                                                 (cons (remove-car (first ints)) ints-acc)
                                                 (cons
                                                  (if (free-next-space? car streets)
                                                      (begin
                                                        (log-move-car (car-id car) #f)
                                                        (log-info (string-append "Car " (number->string (car-id car)) "leaving crossing."))
                                                        (leave-intersection car))
                                                      car)   
                                                  to-move-car))
                         (try-move-crossings-aux (rest ints)
                                                 (cons (first ints) ints-acc)
                                                 to-move-car)))]))
              
          ]
    (try-move-crossings-aux ints empty empty)))

(define (invert-dir dir)
  (list (remainder (add1 (first dir)) 2)
        (remainder (add1 (second dir)) 2)))


;; Block Intersections -> Integer
(define (crossing-block-id block ints)
  (let ([int (next-intersection block ints)])
    (if (equal? (block-dir block) (list 0 1))
        (second (intersection-exit-blocks int))
        (first (intersection-exit-blocks int)))))

;; Block Intersections -> Integer
(define (straight-block-id block ints)
  (let ([int (next-intersection block ints)])
    (if (equal? (block-dir block) (list 0 1))
        (first (intersection-exit-blocks int))
        (second (intersection-exit-blocks int)))))


;; Block Car Intersections -> Intersections
(define (enter-intersection block car ints)
  (let* ( [current-dir (car-dir car)]
          [turn? (< (/ (sub1 (random 1 101)) 100) TURN-RATE)]
          [next-dir (if turn?
                        (invert-dir current-dir)
                        current-dir)]
          (next-block-id (if turn?
                             (crossing-block-id block ints)
                             (straight-block-id block ints)))
          [int (next-intersection block ints)]
          )
    (begin
      (log-move-car (car-id car) #f)
      (log-info (string-append "Car " (number->string (car-id car)) "entering crossing."))
      (if turn? (log-move-car (car-id car) turn?) #f)
      (if turn? (log-info (string-append "Car " (number->string (car-id car)) "turning.")) #f)
      (cons 
       (make-intersection (intersection-id int)
                          (intersection-h-street int)
                          (intersection-v-street int)
                          (intersection-entry-blocks int)
                          (intersection-exit-blocks int)
                          (make-car
                           (car-id car)
                           (car-block-id car)
                           next-dir
                           (car-total-time car)
                           #t
                           next-block-id)                      
                          (intersection-semaphore int))
       (remove int ints (λ (i1 i2) (equal? (intersection-id i1) (intersection-id i2))))
       ))))

(define (try-flow-blocked-lane v normal-flow?)
  (local [
          (define (try-flow-blocked-lane-aux v i)
            (cond [(< i 1) v]
                  [else
                   (try-flow-blocked-lane-aux
                    (if (or normal-flow? (false? (vector-ref v i)))
                        (begin 
                          (for ([j (in-range (sub1 i) -1 -1)]
                                #:when (not (false? (vector-ref v j))))                                         
                            (log-move-car (car-id (vector-ref v j)) #f)
                            )
                          (vector-append (vector #f) (vector-take v i) (vector-drop v (add1 i))))
                        v)
                    (if (or normal-flow? (false? (vector-ref v i)))
                        0
                        (sub1 i)))]))

          ]
    (try-flow-blocked-lane-aux v 4)))
                   

;; Block -> (Block, cars)
(define (move-cars block ints)
  (let* ([moving-out (last (vector->list (block-lane block)))]
         [next-int (next-intersection block ints)]
         [not-blocked-intersection? (and (not (false? moving-out)) (not (false? next-int))
                                         (not (intersection-closed next-int (block-dir block))))]
         [blocked-intersection? (and (not (false? moving-out)) (not (false? next-int))
                                     (intersection-closed next-int (block-dir block)))]
         [exiting? (and (not (false? moving-out)) (false? next-int))]
         [normal-flow? (or not-blocked-intersection?
                           (false? moving-out))]
         [int-entered (if not-blocked-intersection? (enter-intersection block moving-out ints) ints)]
                           
         )
    (list (try-flow-blocked-lane (block-lane block) normal-flow?)                
          (cond [not-blocked-intersection?
                 int-entered]
                [exiting?
                 (begin
                   (log-exit-car (car-id moving-out))
                   ints)]
                [else ints]))))


;; Block -> (Block, Car|#f)
(define (move-block block ints)
  (let ([trying (move-cars block ints)])
    
    (list (make-block (block-id block)
                      (first trying)
                      (block-dir block)
                      (block-sense block)
                      )
          (second trying))))


;; Cars Block -> Block
(define (try-move-int-cars block int-cars)
  (cond [(empty? int-cars) block]
        [(equal? (car-next-block-id (first int-cars)) (block-id block))
         (let ([lane (block-lane block)]
               [car (make-car
                     (car-id (first int-cars))
                     (car-block-id (first int-cars))
                     (car-dir (first int-cars))
                     (car-total-time (first int-cars))
                     #f
                     (car-next-block-id (first int-cars)))])
         (begin
           (if (car-in-intersection? (first int-cars))              
               (begin
                 (log-move-car (car-id car) #f)
                 (log-info (string-append "Car " (number->string (car-id car)) " leaving crossing"))
                 )
               #f)          
           (make-block
            (block-id block)
            (vector-append (vector car) (vector-drop lane 1))
            (block-dir block)
            (block-sense block))))]
        [else
         (try-move-int-cars block (rest int-cars))]))
          

;; Blocks -> (Blocks, cars)
(define (move-blocks blocks ints int-cars)
  (local [
          (define (move-blocks-aux blocks ints blocks-acc)
            (cond [(empty? blocks) (list blocks-acc ints)]
                  [else
                   (let ([trying (move-block (first blocks) ints)])
                     (move-blocks-aux (rest blocks)
                                      (second trying) 
                                      (cons (try-move-int-cars (first trying) int-cars) blocks-acc)
                                      ))]))

          ]
    (move-blocks-aux blocks ints empty)))

(define (block<? b1 b2)
  (< (second (block-id b1)) (second (block-id b2))))

;; Street -> Street
(define (try-move-street street ints int-cars)
  (let* ([moving (move-blocks
                  ((if (or
                       (equal? (street-sense street) '(1 0))
                       (equal? (street-sense street) '(0 1)))
                       reverse
                       values)
                       (sort (street-blocks street) block<?))
                  ints int-cars)]
         [blocks-moved (first moving)]
         [new-ints (second moving)])
    (list (make-street (street-id street)
                       (street-dir street)
                       (street-sense street)
                       blocks-moved
                       (street-entry-block street)
                       (street-exit-block street))
          new-ints)))

;; List[Street] -> (List[Street] List[car])
(define (try-move-streets streets ints int-cars)
  (local [
          (define (try-move-streets-aux streets ints str-acc)
            (cond [(empty? streets) (list str-acc ints)]
                  [else
                   (let ([moving (try-move-street (first streets) ints int-cars)])
                     (try-move-streets-aux (rest streets)
                                           (second moving)  ;ints modificado
                                           (cons (first moving) str-acc)
                                           ))]
                  ))]
    ;(cons (second moving) to-move-out)))])
      
    (try-move-streets-aux streets ints empty)))

(define TIMES (list 47 38 5))

;;TrafficLight -> TrafficLight
(define (next-tf tf)
  (remainder (add1 tf) 3))
  

;;Semaphore -> Semaphore
(define (tick-semaphore sem int)
  (let* ([new-timer (list (sub1 (first (semaphore-timer sem)))
                          (sub1 (second (semaphore-timer sem))))]
         [new-tfs (list (if (< (first new-timer) 0)
                            (next-tf (first (semaphore-traffic-lights sem)))
                            (first (semaphore-traffic-lights sem)))
                        (if (< (second new-timer) 0)
                            (next-tf (second (semaphore-traffic-lights sem)))
                            (second (semaphore-traffic-lights sem)))) ;;REFATORAR COM MAP!!
                  ])
    (begin
      (if (or (< (first new-timer) 0) (< (second new-timer) 0))
          (log-semaphore new-tfs new-timer  int) #f)
      ;(display sem)
      (make-semaphore
       new-tfs
       (list
        (if (< (first new-timer) 0)
            (list-ref TIMES (first new-tfs))
            (first new-timer))
        (if (< (second new-timer) 0)
            (list-ref TIMES (second new-tfs))
            (second new-timer))
        )))))
         
                   
;; Intersection -> Intersection
(define (tick-intersection int)
  (make-intersection (intersection-id int)
                     (intersection-h-street int)
                     (intersection-v-street int)
                     (intersection-entry-blocks int)
                     (intersection-exit-blocks int)
                     (intersection-crossing int)
                     (tick-semaphore (intersection-semaphore int) int)))
  

;; Intersections -> Intersections
(define (tick-intersections ints)
  (map tick-intersection ints))


(define (insert-last-lane lane c)
  (vector-append (vector c) (vector-take-right lane 4)))   

;; Streets Cars -> Streets
(define (insert-out-crossing-cars streets cars)
  (local [
          
          (define (traverse-block b c)
            (if (equal? (car-next-block-id c) (block-id b))
                       (make-block
                        (block-id b)
                        (insert-last-lane (block-lane b) c)
                        (block-dir b)
                        (block-sense b))
                b))
          
          (define (traverse-blocks blocks c)
            (map (λ (b) (traverse-block b c)) blocks))
          
          (define (traverse-street str c)
            (make-street
             (street-id str)
             (street-dir str)
             (street-sense str)
             (traverse-blocks (street-blocks str) c)
             (street-entry-block str)
             (street-exit-block str)
             ))

          (define (insert-out-crossing-car streets car)
            (cond [(empty? streets) empty]
                  [else
                   (cons
                    (traverse-street (first streets) car)                    
                    (insert-out-crossing-car (rest streets)
                                             car))]
                  ))
                                           
          (define (insert-out-crossing-cars-aux streets cars)
            (cond [(empty? cars) streets]
                  [else
                   (insert-out-crossing-cars-aux
                    (insert-out-crossing-car streets (first cars))
                    (rest cars))]))
                                                           
            
          ]
    (insert-out-crossing-cars-aux streets cars)))



(define (insert-car street car-id)
  (local [
          (define (insert-car-entry-block blocks car-id)
            (cond [(empty? blocks) empty]
                  [else
                   (cons
                    (if (equal? (block-id (first blocks)) (street-entry-block street))
                        (begin
                          (log-new-car car-id street)
                          (make-block
                           (block-id (first blocks))
                           (insert-last-lane (block-lane (first blocks))
                                             (make-car
                                              car-id
                                              (block-id (first blocks))
                                              (block-dir (first blocks))
                                              0
                                              #f
                                              #f))
                           (block-dir (first blocks))
                           (block-sense (first blocks))))
                        (first blocks))
                    (insert-car-entry-block (rest blocks) car-id))]))                                                                                   
          ]
  
    (make-street
     (street-id street)
     (street-dir street)
     (street-sense street)
     (insert-car-entry-block (street-blocks street) car-id)
     (street-entry-block street)
     (street-exit-block street))))
    
(define (insert-cars streets car-id)
  (cond [(empty? streets) empty]
        [else
         (cons
          (insert-car (first streets) car-id)
          (insert-cars (rest streets) (add1 car-id)))]
        ))


;; Simulator -> Simulator
(define (tick sim)
  (begin
    (log-clock)
    (let* (
           [ticked-ints (tick-intersections (simulator-intersections sim))]
         
           [try-move-cross (try-move-crossings ticked-ints (append (simulator-h-streets sim) (simulator-v-streets sim))) ]
           [intersections-moved1 (first try-move-cross)]
           [to-move-out-crossing (second try-move-cross)]
         
           [try-move-h-street (try-move-streets (simulator-h-streets sim) intersections-moved1 to-move-out-crossing)]
           [h-streets-moved1 (first try-move-h-street)]
           [intersections2 (second try-move-h-street)]

           [try-move-v-street (try-move-streets (simulator-v-streets sim) intersections2 to-move-out-crossing)]
           [v-streets-moved1 (first try-move-v-street)]
           [intersections3 (second try-move-v-street)]
         
           [insert-car? (= (remainder (simulator-count-iter sim) 10) 0)]
           [v-next-id (+ 10 (simulator-car-id sim))]
           )
      
      (begin
        (make-simulator
         (if insert-car?
             (insert-cars h-streets-moved1 (simulator-car-id sim) )
             h-streets-moved1 )
         (if insert-car?
             (insert-cars v-streets-moved1 v-next-id)
             v-streets-moved1)
         intersections3
         (add1 (simulator-count-iter sim))
         (if insert-car? (+ 20 (simulator-car-id sim)) (simulator-car-id sim))
         )))))


;;; Simulator -> Image
(define (draw sim) empty-image)
(define T-START (current-milliseconds))
(main SIMT1)
(display (- (current-milliseconds) T-START))


#lang racket

(require racket/stream)
(require racket/list)

;; CONSTANTES:

(define COUNT-STREETS 20)
(define C-STREET-HALF (/ COUNT-STREETS 2))
(define TURN-RATE 0.1)
(define BLOCK-LENGTH 5)

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

;; FUNCOES AUXILIARES:


(define (car-id-generator)
  (in-range +inf.0))

(define (cria-pares item lista)
       (map (lambda (item2) (list item item2)) lista))

(define (produto-cartesiano list1 list2)
  (cond [(empty? list1) empty]
        [else
         (append (cria-pares (first list1) list2)
                 (produto-cartesiano (rest list1) list2))]))


;; DEFINIÇÃO DE DADOS:


(define-struct car (id block-id dir total-time in-intersection? next-block-id) #:transparent)
;; Car é (make-car Integer+ Block Integer+ Boolean)
;exemples:
(define CAR1 (make-car 1 (list 0 0) (list 0 1) 2 #f #f))


;(define-struct block (id lane previous-intersection next-intersection))
(define-struct block (id lane dir) #:transparent)
;; Block é (make-block '(Integer+ . Integer+) Vector Intersection Intersection
;examples:
(define BLOCK1 (make-block (list 0 0) (vector #f CAR1 #f #f #f) (list 0 1)))
(define BLOCK2 (make-block (list 10 0) (make-vector 5 #f) (list 1 0)))

(define (create-block street-id id dir)
  (make-block (list street-id id)
              (make-vector 5 #f)
              dir
              ))
              
(define (create-blocks street-id dir)
  (map (lambda (x) (create-block street-id x dir))
       (stream->list (in-range (add1 C-STREET-HALF)))))

(define (next-intersection block ints)
  (for/first ([int ints]
              #:when (or (equal? (intersection-id int) (block-id block))
                     (equal? (intersection-id int) (list
                                           (second (block-id block))
                                           (- (first (block-id block)) C-STREET-HALF)))))
           int)
  )

(define (previous-intersection block intersections)
  (for/first ([int intersections]
              #:when (or (= (intersection-id) (list (first (block-id block))
                                                    (sub1 (second (block-id block)))))
                     (= (intersection-id) (list
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
         [calc-sense (lambda (x) (if (= x 0) 0
                                     (if (= (remainder id 2) 0) 1
                                         -1)))]
         [sense (list (calc-sense (first dir))
                       (calc-sense (second dir)))]
         [blocks (create-blocks id dir) ]
         )
    (make-street id
                 dir
                 sense
                 blocks
                 (if (or (= (first sense) 1) (= (second sense) 1))
                     (first blocks)
                     (last blocks))
                 (if (or (= (first sense) -1) (= (second sense) -1))
                     (first blocks)
                     (last blocks)))
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
                                     )))
                     (list (block-id (list-ref (street-blocks h-street)
                                     (if (equal? (street-sense h-street) (list 0 1))
                                         (add1 (- (street-id v-street) (/ COUNT-STREETS 2) ))
                                         (- (street-id v-street) (/ COUNT-STREETS 2) ))))
                           (block-id (list-ref (street-blocks v-street)
                                     (if (equal? (street-sense v-street) (list 1 0))
                                         (add1 (street-id h-street))
                                         (street-id h-street)
                                         ))))
                     #f
                     (make-semaphore (list RED RED) (list 1 0))))


;; List[Street] -> List[Intersection]
(define (create-intersections h-streets v-streets)
  (let* ([street-pairs (produto-cartesiano h-streets v-streets)]
         [intersections
          (map (lambda (p) (create-intersection (first p) (second p)))
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
    
  


(define-struct simulator (h-streets v-streets intersections) #:transparent)
;;Simulator é (make-simulator List[Street] List[Intersection] Logger) 
;example:
(define SIM1 (make-simulator (list STR1) (list STR2) (list INT1)))

(define (init-simulator)
  (let* (
         [h-streets
          (map (lambda (i) (create-street i (first DIRECTIONS))) (stream->list (in-range 10)))
          ]
         [v-streets
           (map (lambda (i) (create-street i (second DIRECTIONS))) (stream->list (in-range 10 20)))
           ]
         )
         
    (make-simulator
     h-streets
     v-streets
     (create-intersections h-streets v-streets)
     )
    ))






;teste estrutura

(define SIMT1 (init-simulator))
SIMT1


(require 2htdp/universe)
(require 2htdp/image)
;; Big-Bang (ticks):

(define (main sim)
  (big-bang sim
            (on-tick tick)
            (to-draw draw)
            ))


;; Intersection -> Intersection
(define (remove-car int)
  (make-intersection (intersection-id int)
                     (intersection-h-street int)
                     (intersection-v-street int)
                     (intersection-entry-blocks int)
                     (intersection-exit-blocks int)
                     #f
                     (intersection-semaphore int)))
   

;; List[Intersection] -> (List[Intersection], List[car])
(define (try-move-crossings ints)
  (local [
    (define (try-move-crossings-aux ints ints-acc to-move-car)
      (cond [(empty? ints) (list ints-acc to-move-car)]
            [else
             (let ([car (intersection-crossing (first ints))])
              (if (not (false? car))
                      (try-move-crossings-aux (rest ints)
                                          (cons (remove-car (first ints)) ints-acc)
                                          (cons car to-move-car))
                      (try-move-crossings-aux (rest ints)
                                          (cons (first ints) ints-acc)
                                          to-move-car)))]))
              
    ]
    (try-move-crossings-aux ints empty empty)))

(define (vector-shift v)
    (build-vector 5
                  (lambda (i)
                    (if (= i 0)
                        #f
                        (vector-ref v (- i 1)))))
  )
                           

(define (invert-dir dir)
  (list (remainder (add1 (first dir)) 2)
        (remainder (add1 (second dir)) 2)))


;; Block Intersections -> Integer
(define (crossing-block-id block)
  (let ([int (next-intersection block ints)])
    (if (= (block-dir block) (list 0 1))
        (second (intersection-exit-blocks int))
        (first (intersection-exit-blocks int)))))

;; Block Intersections -> Integer
(define (straight-block-id block)
  (let ([int (next-intersection block ints)])
    (if (= (block-dir block) (list 0 1))
        (first (intersection-exit-blocks int))
        (second (intersection-exit-blocks int)))))


;; Block Car Intersections -> Intersections
(define (enter-intersection block car ints)
  (let* ( [current-dir (car-dir car)]
         [next-dir (if (< (/ (sub1 (random 1 101)) 100) TURN-RATE)
                       (invert-dir current-dir)
                       current-dir)]
         (next-block-id (if (not (equal? next-dir current-dir))
                            (crossing-block-id block)
                            (straight-block-id block)))
         [int (next-intersection block ints)]
         )
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
     (remove int ints (lambda (i) (= (intersection-id i) (intersection-id int))))
    )))
     

;; Car -> Log
; !!!
(define (exit-car car) (display "carro saiu"))


;; Block -> (Block, cars)
(define (move-cars block ints)
  (let* ([moving-out (last (vector->list (block-lane block)))]
         [next-int (next-intersection block ints)]
         )
    (list (vector-shift (block-lane block)) ;;incluir mudança do timer dos carros, assim como log car-move
          (cond [(and (not (false? moving-out)) (not (false? next-int)))
                 (enter-intersection moving-out ints)]
                [(not (false? moving-out))
                 (begin (exit-car moving-out) ints)]
                [else ints]))))


;; Block -> (Block, Car|#f)
(define (move-block block ints)
  (let* ([next-int (next-intersection block ints)]
         [trying
          (if (and (not (false? next-int))
                   (intersection-closed next-int (block-dir block)))
              (list (block-lane block) ints)
              (move-cars block ints))])
    
  (list (make-block (block-id block)
                    (first trying)
                    (block-dir block)
                  )
        (second trying))))

;; Blocks -> (Blocks, cars)
(define (move-blocks blocks ints)
  (local [
    (define (move-blocks-aux blocks ints blocks-acc)
      (cond [(empty? blocks) (list blocks-acc ints)]
            [else
             (let ([trying (move-block (first blocks) ints)])
             (move-blocks-aux (rest blocks)
                              (second trying) ;ints modificado
                              (cons (first trying) blocks-acc)))]))
                              ;(if (false? (second trying))
                                 ; to-move-out
                                 ; (cons (second trying) to-move-out))))])
      ]
  (move-blocks-aux blocks ints empty)))


;; Street -> Street
(define (try-move-street street ints)
  (let* ([moving (move-blocks (street-blocks street) ints)]
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
(define (try-move-streets streets ints)
  (local [
    (define (try-move-streets-aux streets ints str-acc)
      (cond [(empty? streets) (list str-acc ints)]
            [else
             (let ([moving (try-move-street (first streets) ints)])
             (try-move-streets-aux (rest streets)
                                   (second moving)  ;ints modificado
                                   (cons (first moving) str-acc)))]
            ))]
                                   ;(cons (second moving) to-move-out)))])
      
  (try-move-streets-aux streets ints empty)))

(define TIMES (list 47 38 5))

;;TrafficLight -> TrafficLight
(define (next-tf tf)
  (remainder (add1 tf) 3))
  

;;Semaphore -> Semaphore
(define (tick-semaphore sem)
  (let* ([new-timer (list (sub1 (first (semaphore-timer sem)))
                         (sub1 (second (semaphore-timer sem))))]
        [new-tfs (list (if (< (first new-timer) 0)
                           (next-tf (first (semaphore-traffic-lights sem)))
                           (first (semaphore-traffic-lights sem)))
                       (if (< (second new-timer) 0)
                           (next-tf (second (semaphore-traffic-lights sem)))
                           (second (semaphore-traffic-lights sem))))
                 ])
                               
    (make-semaphore
     new-tfs
     (list
      (if (< (first new-timer) 0)
          (list-ref TIMES (first new-tfs))
          (first new-timer))
      (if (< (second new-timer) 0)
          (list-ref TIMES (second new-tfs))
          (second new-timer))
      ))))
         
     
               

;; Intersection -> Intersection
(define (tick-intersection int)
  (make-intersection (intersection-id int)
                     (intersection-h-street int)
                     (intersection-v-street int)
                     (intersection-entry-blocks int)
                     (intersection-exit-blocks int)
                     (intersection-crossing int)
                     (tick-semaphore (intersection-semaphore int))))
  

;; Intersections -> Intersections
(define (tick-intersections ints)
  (map tick-intersection ints))


;; Streets Car -> Street
(define (insert-out-crossing-car streets car)
  (let* ([street (get-street (car-next-block car) streets)]
         [block (get-block (car-next-block car) street)]
         )
    (
    

;; Streets Cars -> Streets
(define (insert-out-crossing-cars streets cars)
  (local [
          (define (traverse-street str c)
            (
            
          
          (define (insert-out-crossing-cars-aux streets car streets-aux)
            (cond [(empty? streets) streets-aux]
                  [else
                   (insert-out-crossing-cars-aux (rest streets)
                                                 car
                                                 (traverse-street (first streets) car))]))
                                                           
            
          ]
  (map (lambda (c) (insert-out-crossing-cars-aux streets c empty)) cars))) 


;;; Simulator -> Simulator

;(define (tick sim) sim)

(define (tick sim)
  (let* (
         [ticked-ints (tick-intersections (simulator-intersections sim))]
         
         [try-move-cross (try-move-crossings ticked-ints)]
         [intersections-moved1 (first try-move-cross)]
         [to-move-out-crossing (second try-move-cross)]
         
         [try-move-h-street (try-move-streets (simulator-h-streets sim) intersections-moved1)]
         [h-streets-moved1 (first try-move-h-street)]
         [intersections2 (second try-move-h-street)]

         [try-move-v-street (try-move-streets (simulator-v-streets sim) intersections2)]
         [v-streets-moved1 (first try-move-v-street)]
         [intersections3 (second try-move-v-street)]
         )
  (make-simulator
   (insert-out-crossing-cars h-streets-moved1 to-move-out-crossing)
   (insert-out-crossing-cars v-streets-moved1 to-move-out-crossing)
   intersections3 )
   
  ))


;;; Simulator -> Image
(define (draw sim) empty-image)
(main SIMT1)


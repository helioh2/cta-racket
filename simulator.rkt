#lang racket

(require racket/stream)
(require racket/list)

;; CONSTANTES:

(define COUNT-STREETS 20)
(define C-STREET-HALF (/ COUNT-STREETS 2))
(define TURN-RATE 0.1)
(define BLOCK-LENGTH 5)

(define SENSES (list (cons 0 1) (cons 0 -1) (cons 1 0) (cons -1 0)))
(define SENSES_NAMES (list "RIGHT" "LEFT" "DOWN" "UP"))

(define DIRECTIONS (list (cons 0 1) (cons 1 0)))
(define DIRECTIONS_NAMES (cons "HORIZONTAL" "VERTICAL"))

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


(define-struct carro (id block-id total-time in-intersection?) #:transparent)
;; Car é (make-car Integer+ Block Integer+ Boolean)
;exemples:
(define CAR1 (make-carro 1 (cons 0 0) 2 #f))


;(define-struct block (id lane previous-intersection next-intersection))
(define-struct block (id lane dir) #:transparent)
;; Block é (make-block '(Integer+ . Integer+) Vector Intersection Intersection
;examples:
(define BLOCK1 (make-block (cons 0 0) (vector #f CAR1 #f #f #f) (cons 0 1)))
(define BLOCK2 (make-block (cons 10 0) (make-vector 5 #f) (cons 1 0)))

(define (create-block street-id id dir)
  (make-block (cons street-id id)
              (make-vector 5 #f)
              dir
              ))
              
(define (create-blocks street-id dir)
  (map (lambda (x) (create-block street-id x dir))
       (stream->list (in-range (add1 C-STREET-HALF)))))

(define (next-intersection block ints)
  (for/first ([int ints]
              #:when (or (equal? (intersection-id int) (block-id block))
                     (equal? (intersection-id int) (cons
                                           (cdr (block-id block))
                                           (- (car (block-id block)) C-STREET-HALF)))))
           int)
  )

(define (previous-intersection block intersections)
  (for/first ([int intersections]
              #:when (or (= (intersection-id) (cons (first (block-id block))
                                                    (sub1 (second (block-id block)))))
                     (= (intersection-id) (cons
                                           (sub1 (second (block-id block)))
                                           (- (first (block-id block)) C-STREET-HALF)))))
           int)
  )
  

(define-struct street (id dir sense blocks entry-block exit-block) #:transparent)
;; Street é (make-street Integer+ Integer[0,1] PairInSENSES List[Block] Function Block Block)
;example
(define STR1 (make-street 0 0 (cons 0 1) (list BLOCK1) BLOCK1 #f))
(define STR2 (make-street 1 1 (cons 1 0) (list BLOCK2) BLOCK2 #f))



(define (create-street id dir)
  (let* (
         [calc-sense (lambda (x) (if (= x 0) 0
                                     (if (= (remainder id 2) 0) 1
                                         -1)))]
         [sense (cons (calc-sense (car dir))
                       (calc-sense (cdr dir)))]
         [blocks (create-blocks id dir) ]
         )
    (make-street id
                 dir
                 sense
                 blocks
                 (if (or (= (car sense) 1) (= (cdr sense) 1))
                     (first blocks)
                     (last blocks))
                 (if (or (= (car sense) -1) (= (cdr sense) -1))
                     (first blocks)
                     (last blocks)))
    )
    )

;; Street inicial é (create-street)


;;TrafficLight é um desses: RED, GREEN, YELLOW

(define-struct semaphore (traffic-lights timer) #:transparent)
;; Semaphore é (make-semaphore Pair(TrafficLight) Integer+)
;example:
(define SEM1 (make-semaphore (cons RED RED) (cons 0 1)))


(define-struct intersection (id h-street v-street entry-blocks exit-blocks crossing semaphore) #:transparent)
;; Intersection é (make-intersection '(Integer+,Integer+) Street Street Pair[Block] Pair[Block] Car|#false Semaphore)
;example
(define INT1 (make-intersection (cons 0 0) STR1 STR2 (cons BLOCK1 #f) (cons BLOCK2 #f) #f SEM1))


;; Street -> Intersection
(define (create-intersection h-street v-street)
  (make-intersection (cons (street-id h-street) (- (street-id v-street) (/ COUNT-STREETS 2)))
                     (street-id h-street)
                     (street-id v-street)
                     (cons (block-id (list-ref (street-blocks h-street)
                                     (if (equal? (street-sense h-street) (cons 0 1))
                                         (- (street-id v-street) (/ COUNT-STREETS 2) )
                                         (add1 (- (street-id v-street) (/ COUNT-STREETS 2) )))))                                         
                                     
                           (block-id (list-ref (street-blocks v-street)
                                     (if (equal? (street-sense v-street) (cons 1 0))
                                         (street-id h-street)
                                         (add1 (street-id h-street)))
                                     )))
                     (cons (block-id (list-ref (street-blocks h-street)
                                     (if (equal? (street-sense h-street) (cons 0 1))
                                         (add1 (- (street-id v-street) (/ COUNT-STREETS 2) ))
                                         (- (street-id v-street) (/ COUNT-STREETS 2) ))))
                           (block-id (list-ref (street-blocks v-street)
                                     (if (equal? (street-sense v-street) (cons 1 0))
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

;; Intersection Carro -> Boolean
(define (intersection-closed int dir)
  (let* ([index-dir (car dir)]
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
   

;; List[Intersection] -> (List[Intersection], List[Carro])
(define (try-move-crossings ints)
  (local [
    (define (try-move-crossings-aux ints ints-acc to-move-car)
      (cond [(empty? ints) (cons ints-acc to-move-car)]
            [else
             (let ([carro (intersection-crossing (first ints))])
              (if (not (false? carro))
                  ;(if (can-move-car-out-crossing carro (intersection-exit-blocks))
                      (try-move-crossings-aux (rest ints)
                                          (cons (remove-car (first ints)) ints-acc)
                                          (cons carro to-move-car))
                      ;(cons (first ints) (cons carro not-moved) to-add))
                      ;(cons (first ints) not-moved to-add))
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
                           
          

;; Block -> (Block, Carros)
;; !!!
(define (move-cars block)
  (let* ([moving-out (last (vector->list (block-lane block)))])
    (cons (vector-shift (block-lane block)) moving-out)))


;; Block -> (Block, Car|#f)
(define (move-block block ints)
  (let* ([next-int (next-intersection block ints)]
         [trying
          (if (and (not (false? next-int))
                   (intersection-closed next-int (block-dir block)))
              (cons (block-lane block) #f)
              (move-cars block))])
  (cons (make-block (block-id block)
                    (car trying)
                    (block-dir block)
                  )
        (cdr trying))))

;; Blocks -> (Blocks, Carros)
(define (move-blocks blocks ints)
  (local [
    (define (move-blocks-aux blocks ints blocks-acc to-move-out)
      (cond [(empty? blocks) (cons blocks-acc to-move-out)]
            [else
             (let ([trying (move-block (first blocks) ints)])
             (move-blocks-aux (rest blocks) ints
                              (cons (car trying) blocks-acc)
                              (if (false? (cdr trying))
                                  to-move-out
                                  (cons (cdr trying) to-move-out))))])
      )]
  (move-blocks-aux blocks ints empty empty)))


;; Street -> Street
(define (try-move-street street ints)
  (let* ([moving (move-blocks (street-blocks street) ints)]
         [blocks-moved (car moving)]
         [cars-out (cdr moving)])
  (cons (make-street (street-id street)
               (street-dir street)
               (street-sense street)
               blocks-moved
               (street-entry-block street)
               (street-exit-block street))
    cars-out)))

;; List[Street] -> (List[Street] List[Carro])
(define (try-move-streets streets ints)
  (local [
    (define (try-move-streets-aux streets ints str-acc to-move-out)
      (cond [(empty? streets) (cons str-acc to-move-out)]
            [else
             (let ([moving (try-move-street (first streets) ints)])
             (try-move-streets-aux (rest streets) ints
                                   (cons (car moving) str-acc)
                                   (cons (cdr moving) to-move-out)))])
      )]
  (try-move-streets-aux streets ints empty empty)))

(define TIMES (list 47 38 5))

;;Semaphore -> Semaphore
(define (tick-semaphore sem)
  (let ([new-timer (list (sub1 (first (semaphore-timer sem)))
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
      )))
         
     
               

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

;;; Simulator -> Simulator

;(define (tick sim) sim)

(define (tick sim)
  (let* (
         [ticked-ints (tick-intersections (simulator-intersections sim))]
         [try-move-cross (try-move-crossings (simulator-intersections sim))]
         [intersections-moved1 (car try-move-cross)]
         [to-move-out-crossing (cdr try-move-cross)]
         
         [try-move-street (try-move-streets (append (simulator-h-streets sim)
                                                    (simulator-v-streets sim))
                                            (simulator-intersections sim)
                                            )]
         [streets-moved1 (car try-move-street)]
         [to-move-in-crossing (cdr try-move-street)]
         )
  (make-simulator
   
  )))


;;; Simulator -> Image
(define (draw sim) empty-image)
(main SIMT1)


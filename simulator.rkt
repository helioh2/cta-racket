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
(define-struct block (id lane) #:transparent)
;; Block é (make-block '(Integer+ . Integer+) Vector Intersection Intersection
;examples:
(define BLOCK1 (make-block (cons 0 0) (vector #f CAR1 #f #f #f)))
(define BLOCK2 (make-block (cons 10 0) (make-vector 5 #f)))

(define (create-block street-id id)
  (make-block (cons street-id id)
              (make-vector 5 #f)
              ))
              
(define (create-blocks street-id)
  (map (lambda (x) (create-block street-id x))
       (stream->list (in-range (add1 C-STREET-HALF)))))

(define (next-intersection block intersections)
  (for/first ([int intersections]
              #:when (or (= (intersection-id) (block-id))
                     (= (intersection-id) (cons
                                           (second (block-id block))
                                           (- (first (block-id)) C-STREET-HALF)))))
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
  

(define-struct street (id direction sense blocks entry-block exit-block) #:transparent)
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
         [blocks (create-blocks id) ]
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
(define SEM1 (make-semaphore (cons RED RED) 0))


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
                     (make-semaphore (cons RED RED) 0)))


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
            (on-tick (tick sim))
            (to-draw (draw sim))
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


;; Block -> (Block, Carros)
;; !!!
(define (move-cars block)
  0)
  
  
  

;; Block -> (Block, Car|#f)
(define (move-block block)
  (let* ([trying
          (if (intersection-closed (next-intersection block))
              (cons (block-lane block) #f)
              (move-cars block))])
  (cons (make-block (block-id block)
                    (car trying)
                  )
        (cdr trying))))

;; Blocks -> (Blocks, Carros)
(define (move-blocks blocks)
  (local [
    (define (move-blocks-aux blocks blocks-acc to-move-out)
      (cond [(empty? blocks) (cons blocks-acc to-move-out)]
            [else
             (let ([trying (move-block (first blocks))])
             (move-blocks-aux (rest blocks)
                              (cons (car trying) blocks-acc)
                              (if (false? (cdr trying))
                                  to-move-out
                                  (cons (cdr trying) to-move-out))))])
      )])
  (move-blocks-aux blocks empty empty))


;; Street -> Street
(define (try-move-street street)
  (let* ([moving (move-blocks (street-blocks street))]
         [blocks-moved (car moving)]
         [cars-out (cdr moving)])
  (cons (make-street (street-id street)
               (street-dir street)
               (street-sense street)
               blocks-moved
               (street-entry-block street)
               (street-exit-block street)))
    cars-out))

;; List[Street] -> (List[Street] List[Carro])
(define (try-move-streets streets)
  (local [
    (define (try-move-streets-aux streets str-acc to-move-out)
      (cond [(empty? streets) (cons str-acc to-move-out)]
            [else
             (let ([moving (try-move-street (first streets))])
             (try-move-streets-aux (rest streets)
                                   (cons (car moving) str-acc)
                                   (cons (cdr moving) to-move-out)))])
      )])
  (try-move-streets-aux streets empty empty))
             


;;; Simulator -> Simulator

;(define (tick sim) sim)

(define (tick sim)
  (let* (
         [try-move-cross (try-move-crossings (simulator-intersections sim))]
         [intersections-moved1 (car try-move-cross)]
         [to-move-out-crossing (cdr try-move-cross)]
         
         [try-move-street (try-move-streets (append (simulator-h-streets sim)
                                                    (simulator-v-streets sim)))]
         [streets-moved1 (car try-move-street)]
         [to-move-in-crossing (cdr try-move-street)]
         )
  (make-simulator
   
  )))


;;; Simulator -> Image
(define (draw sim) empty-image)



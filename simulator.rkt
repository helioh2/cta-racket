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

(define (street-id-generator)
  (in-range 20))

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


(define-struct car (id block-id total-time in-intersection?))
;; Car é (make-car Integer+ Block Integer+ Boolean)
;exemples:
(define CAR1 (make-car 1 (cons 0 0) 2 #f))


;(define-struct block (id lane previous-intersection next-intersection))
(define-struct block (id lane))
;; Block é (make-block '(Integer+ . Integer+) Vector Intersection Intersection
;examples:
(define BLOCK1 (make-block (cons 0 0) (vector #f CAR1 #f #f #f)))
(define BLOCK2 (make-block (cons 10 0) (make-vector 5 #f)))

(define (create-block street-id id)
  (make-block (cons street-id id)
              (make-vector 5 #f)
              #f #f))
              
(define (create-blocks street-id)
  (map (lambda (x) (create-block street-id x))
       (in-range (add1 C-STREET-HALF))))

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
  

(define-struct street (id direction sense blocks block-id-generator entry-block exit-block))
;; Street é (make-street Integer+ Integer[0,1] PairInSENSES List[Block] Function Block Block)
;example
(define STR1 (make-street 0 0 (cons 0 1) (list BLOCK1) (lambda () (in-range +inf.0)) BLOCK1 #f))
(define STR2 (make-street 1 1 (cons 1 0) (list BLOCK2) (lambda () (in-range +inf.0)) BLOCK2 #f))



(define (create-street dir)
  (let* ([id (street-id-generator)]
         [calc-sense (lambda (x) (if (= x 0) 0
                                     (if (= (remainder id 2) 0) 1
                                         -1)))]
         [sense (cons (calc-sense (first dir))
                       (calc-sense (second dir)))]
         [blocks (create-blocks id) ]
         )
    (make-street id
                 dir
                 sense
                 blocks
                 (lambda () (in-range +inf.0))
                 (if (or (= (first sense) 1) (= (second sense) 1))
                     (first blocks)
                     (last blocks))
                 (if (or (= (first sense) -1) (= (second sense) -1))
                     (last blocks)
                     (first blocks)))
    )
    )

;; Street inicial é (create-street)


;;TrafficLight é um desses: RED, GREEN, YELLOW

(define-struct semaphore (traffic-lights timer))
;; Semaphore é (make-semaphore Pair(TrafficLight) Integer+)
;example:
(define SEM1 (make-semaphore (cons RED RED) 0))


(define-struct intersection (id h-street v-street entry-blocks exit-blocks crossing semaphore))
;; Intersection é (make-intersection '(Integer+,Integer+) Street Street Pair[Block] Pair[Block] Car|#false Semaphore)
;example
(define INT1 (make-intersection (cons 0 0) STR1 STR2 (cons BLOCK1 #f) (cons BLOCK2 #f) #f SEM1))


;; Street -> Intersection
(define (create-intersection h-street v-street)
  (make-intersection (cons (street-id h-street) (- (street-id v-street) (/ COUNT-STREETS 2)))
                     h-street
                     v-street
                     (cons (list-ref (street-blocks h-street)
                                     (street-id v-street))
                           (list-ref (street-blocks v-street)
                                     (street-id h-street)))
                     (cons (list-ref (street-blocks h-street)
                                     (add1 (street-id v-street)))
                           (list-ref (street-blocks v-street)
                                     (add1 (street-id h-street))))
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



;(for ([int empty-intersections]
;      [id (intersection-id int)]
;      [pair (cons (findf (lambda (street) (= (first id) (street-id street)))
;                         h-streets)
;                  (findf (lambda (street) (= (second id) (street-id street)))
;                         v-streets))]
;      [make-intersection
;       id
;       (intersection-h-street int)
;       (intersection-v-street int)
;       (
;        [#when (= (second (block-id b1)) (second (block-id b2)))]
;        [(create-intersection b1 b2)])
;       ]))
;]
;
;)
;)


(define-struct simulator (h-streets v-streets intersections))
;;Simulator é (make-simulator List[Street] List[Intersection] Logger) 
;example:
(define SIM1 (make-simulator (list STR1) (list STR2) (list INT1)))

(define (init-simulator)
  (let* (
         [h-streets
          (map (lambda (i) (create-street (first DIRECTIONS))) (stream->list (in-range 10)))
          ]
         [v-streets
           (map (lambda (i) (create-street (second DIRECTIONS))) (stream->list (in-range 10)))
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






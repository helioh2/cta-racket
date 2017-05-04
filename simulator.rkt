#lang racket

(require racket/stream)
(require racket/list)

;; CONSTANTES:

(define COUNT-STREETS 20)
(define TURN-RATE 0.1)
(define BLOCK-LENGTH 5)

(define SENSES (list (cons 0 1) (cons 0 -1) (cons 1 0) (cons -1 0)))
(define SENSES_NAMES (list "RIGHT" "LEFT" "DOWN" "UP"))

(define DIRECTIONS (list (cons 0 1) (cons 1 0)))
(define DIRECTIONS_NAMES ("HORIZONTAL" "VERTICAL"))

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

;; DEFINIÇÃO DE DADOS:

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
         )
    (make-street id
                 dir
                 '((calc-sense (first dir))
                   .
                   (calc-sense (second dir)))
                 empty    ;; Create Blocks Here
                 (lambda () (in-range +inf.0))
                 #f #f)
    ))

;; Street inicial é (create-street)


(define-struct block (id street lane previous-intersection next-intersection))
;; Block é (make-block '(Integer+ . Integer+) List Intersection Intersection
;examples:
(define BLOCK1 ((cons 0 0) STR1 (list #f CAR1 #f #f #f) #f INT1 ))
(define BLOCK2 ((cons 1 0) STR2 (list #f #f #f #f #f) #f INT1 ))


;;TrafficLight é um desses: RED, GREEN, YELLOW

(define-struct semaphore (traffic-lights timer))
;; Semaphore é (make-semaphore Pair(TrafficLight) Integer+)
;example:
(define SEM1 ((cons RED RED) 0))


(define-struct intersection (id h-street v-street entry-blocks exit-blocks crossing semaphore))
;; Intersection é (make-intersection '(Integer+,Integer+) Street Street Pair[Block] Pair[Block] Car|#false Semaphore)
;example
(define INT1 ((cons 0 1) STR1 STR2 (cons BLOCK1 #f) (cons BLOCK2 #f) #f SEM1))


;; Street -> Intersection
(define (create-intersection h-street v-street)
  (make-intersection (cons (street-id h-street) (street-id v-street))
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
  (let* ([street-pairs (cartesian-product h-streets v-streets)]
         [intersections
          (for ([p street-pairs]
                [(create-intersection (first p) (second p))]
                )
            )
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
(define SIM1 (make-simulator (list STR1 STR2) (list INT1)))

(define (init-simulator)
  (let* ([h-streets
          (for ([i (in-range 10)] [(create-street (first DIRECTIONS))]))
          ]
         [(v-streets
           (for ([i (in-range 10)] [(create-street (second DIRECTIONS))]))
           )]
         )
    (make-simulator
     h-streets
     v-streets
     (create-intersections h-streets v-streets)
     )
    ))




(define-struct car (id block total-time in-intersection?))
;; Car é (make-car Integer+ Block Integer+ Boolean)
;exemples:
(define CAR1 (make-car 1 BLOCK1 2 #f))








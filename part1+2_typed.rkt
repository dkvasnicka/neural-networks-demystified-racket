#lang typed/racket

(require math/matrix
         math/array)

; X = (hours sleeping, hours studying), y = Score on test
(define X-abs (matrix [[3 5] [5 1] [10 2]]))
(define y-abs (matrix [[75] [82] [93]]))

; Normalize
(define X (array/ X-abs (array-axis-max X-abs 0)))
(define y (matrix-scale y-abs (/ 1 100)))

(define one (array 1))

(: random-provider (Index Index -> Real))
(define (random-provider i j)
  (random))

(struct neural-network ([W1 : (Matrix Real)] [W2 : (Matrix Real)]))

(: sigmoid ((Array Real) -> (Array Real)))
(define (sigmoid z)
  (array/ one (array+ one (matrix-map exp (matrix- z)))))

(: forward (neural-network (Array Real) -> (Array Real)))
(define (forward nn x)
  (let* ([z2 (matrix* x (neural-network-W1 nn))]
         [a2 (sigmoid z2)]
         [z3 (matrix* a2 (neural-network-W2 nn))])
    (sigmoid z3)))

(forward
  (neural-network
    (build-matrix 2 3 random-provider)
    (build-matrix 3 1 random-provider))
  X)


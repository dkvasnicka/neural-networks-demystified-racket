#lang racket

(require math/matrix
         math/array)

; X = (hours sleeping, hours studying), y = Score on test
(define X-abs (matrix [[3 5] [5 1] [10 2]]))
(define y-abs (matrix [[75] [82] [93]]))

; Normalize
(define X (matrix-scale X-abs (/ 1 (array-all-max X-abs))))
(define y (matrix-scale y-abs (/ 1 100)))

(define one (array 1))
(define (random-provider i j)
  (random))
(struct neural-network (W1 W2))

(define (sigmoid z)
  (array/ one (array+ one (matrix-map exp (matrix- z)))))

(define (forward nn x)
  (let* ([z2 (matrix* x (neural-network-W1 nn))]
         [a2 (sigmoid z2)]
         [z3 (matrix* a2 (neural-network-W2 nn))])
    (sigmoid z3)))

(forward
  (neural-network
    (matrix [[0.3 0.5 0.9] [0.5 0.1 0.4]]) ;(build-matrix 2 3 random-provider)
    (matrix [[0.5] [0.1] [0.2]]) ;(build-matrix 3 1 random-provider)
    )
  X)


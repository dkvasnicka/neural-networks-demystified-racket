#lang racket

(require math/array
         (rename-in math/array
                    [flarray fa]
                    [flarray/ fa/]
                    [flarray- fa-]
                    [flarray+ fa+]
                    [flarray* dot]))

; X = (hours sleeping, hours studying), y = Score on test
(define X-abs (fa #[#[3 5] #[5 1] #[10 2]]))
(define y-abs (fa #[#[75] #[82] #[93]]))

; Normalize
(define X (fa/ X-abs (fa (array-all-max X-abs))))
(define y (fa/ y-abs (fa 100)))

(define one (fa 1))
(struct neural-network (W1 W2))

(define (sigmoid z)
  (fa/ one (fa+ one (flarray-map exp (fa- z)))))

(define (forward nn x)
  (let* ([z2 (dot x (neural-network-W1 nn))]
         [a2 (sigmoid z2)]
         [z3 (dot a2 (neural-network-W2 nn))])
    (sigmoid z3)))

(forward
  (neural-network
    (array->flarray (build-array #[3 2] (λ (i) (random))))
    (array->flarray (build-array #[3 1] (λ (i) (random)))))
  X)


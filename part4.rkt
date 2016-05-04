#lang racket

(require "part1+2.rkt"
         math/array
         math/matrix)

(define (sigmoid-prime z)
  (array/ (matrix-map exp (matrix- z)) 
          (array-sqr (array+ one (matrix-map exp (matrix- z))))))

(define (cost-function nn x yy)
  (let ([yHat (forward nn x)])
    (* 0.5 (array-all-sum (array-sqr (array- yy yHat))))))

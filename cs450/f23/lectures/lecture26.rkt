#lang racket

(define BIG-NUMBER 999999)

;; sum-to : Nat -> Nat
;; Sums the numbers in the interval [0, x]
(define (sum-to x)
  (if (zero? x)
      x
      (+ x (sum-to (sub1 x)))))

;; recursion: slower than iterative?
(time (sum-to BIG-NUMBER))
; cpu time: 202 real time: 201 gc time: 156

;; iteration: faster than recursion?
(time (for/sum ([x (add1 BIG-NUMBER)]) x))
; cpu time: 15 real time: 6 gc time: 0

;; Racket has no iteration! only recursion!
;; (for/sum is a macro for a tail-recursive function!)

;; iterative-sum-to : Nat -> Nat
;; Sums the numbers in the interval [0, x]
;; - with tail calls
(define (iterative-sum-to x result)
  (if (zero? x)
      result
      (iterative-sum-to (sub1 x) (+ x result))))

(time (iterative-sum-to BIG-NUMBER 0))
; cpu time: 15 real time: 13 gc time: 0

;; "iteration" can be same as "recursive", with tail calls
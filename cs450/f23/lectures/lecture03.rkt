#lang racket

;; CS 450, lecture 03: Wed, Sept 13, 2023

;; Racket intro - functions

;; Functions are defined with `define`
;; (`define`s are the only non-expressions you should be using now)
;; The body of a function is an expression (no "return")
(define (f x)
  (+ x 2))

;; But functions are expressions, expressed with lambda
(lambda (x) (+ x 2))

;; In fact, define for functions is merely shorthand
;; for defining a variable that is bound to a lambda

;; equivalent:
;; (define (f x) (+ x 2))
;; (define f (lambda (x) (+ x 2)))

;; Since functions are just expressions,
;; they can be nested with other expressions

;; For example, the first position in a function call
;; can be an expression that evaluates to a function

;; do-nothing: Any -> Any
;; Function that does nothing and returns its argument unchanged
(define (do-nothing x) x)

;; function call with a computed function
((do-nothing f) 10) ; => evaluates to 12

;; Notice that functions can be passed as arguments too
;; here is a fun program that, when run, goes into an infinite loop
;; Can you figure out why???
#;((lambda (f) (f f))
   (lambda (f) (f f)))



;; `big-bang` is a small framework that we will use
;; to write interactive programs

;; It (roughly) follows MVC structure, where programmers
;; specify handlers that manipulate a "WorldState"
;; (which they must define themselves)

;; Here is an example of an animated ball

(require 2htdp/image
         2htdp/universe)

(define WORLD-WIDTH 200)
(define WORLD-HEIGHT 200)

;; A WorldState is a Non-negative Integer
;; Interp: Represents the y Coordinate of the center of a
;;         ball in a `big-bang` animation.
(define (WorldState? x)
  (exact-nonnegative-integer? x))

(define INITIAL-WORLD 0)

(define BALL-RADIUS 50)
(define BALL-X (/ WORLD-WIDTH 2))
(define BALL-IMG
  (circle BALL-RADIUS "solid" "red"))

;; render: WorldState -> Image
;; Draws a WorldState as a 2htdp/image Image
(define (render w)
  (place-image
   BALL-IMG
   BALL-X w
   (empty-scene WORLD-WIDTH WORLD-HEIGHT)))

;; next-world : WorldState -> WorldState
;; Each tick increments y pos by 1
(define (next-world w)
  (add1 w))

(define (start-world)
  (big-bang INITIAL-WORLD
    [on-tick next-world]
    [to-draw render]))

(start-world)
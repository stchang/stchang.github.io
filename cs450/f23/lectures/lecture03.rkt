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
(define EMPTY-SCENE (empty-scene WORLD-WIDTH WORLD-HEIGHT))

;; A WorldState is a Non-negative Integer
;; Interp: Represents the y Coordinate of the center of a
;;         ball in a `big-bang` animation.
(define (WorldState? x)
  (exact-nonnegative-integer? x))

(define INITIAL-WORLDSTATE 0)

(define BALL-RADIUS 50) ; pixels
(define BALL-X (/ WORLD-WIDTH 2)) ; Coordinate
(define BALL-IMG
  (circle BALL-RADIUS "solid" "red"))

;; render-world: WorldState -> Image
;; Draws a WorldState as a 2htdp/image Image
(define (render-world w)
  (place-image
   BALL-IMG
   BALL-X w
   EMPTY-SCENE))

;; next-worldstate : WorldState -> WorldState
;; Each tick increments y pos by 1
(define (next-worldstate w)
  (add1 w))

(define (start-world)
  (big-bang INITIAL-WORLDSTATE
    [on-tick next-worldstate]
    [to-draw render-world]))

(start-world)
#lang racket

;; CS 450 Fall 2024, section 2
;; Lecture 03: Wed Sept 11, 2024

;; `big-bang` is a small framework that we will use
;; to write interactive programs

;; It (roughly) follows MVC structure, where programmers
;; specify handlers that manipulate a "WorldState"
;; (which they must define themselves)

;; Here is an example of a big-bang program with a moving ball

(require 2htdp/image
         2htdp/universe)

(define WORLD-WIDTH 200)
(define WORLD-HEIGHT 200)
(define EMPTY-SCENE (empty-scene WORLD-WIDTH WORLD-HEIGHT))

;; A WorldState is a Non-negative Integer
;; Interp: Represents the y coordinate of a ball animation.

(define INITIAL-WORLD 0)

(define BALL-RADIUS 50)
(define BALL-X (/ WORLD-WIDTH 2))
(define BALL-IMG
  (circle BALL-RADIUS "solid" "red"))

;; render: WorldState -> Image
;; Adds BALL-IMG to a WORLD-WIDTHxWORLD-HEIGHT empty scene at coordinate (100, w)
(define (render w)
  (place-image BALL-IMG BALL-X w EMPTY-SCENE))

;; next-world : WorldState -> WorldState
;; Each tick increments y pos by 1
(define (next-world w)
  (add1 w))

(define (main)
  (big-bang INITIAL-WORLD
    [on-tick next-world]
    [to-draw render]))

;(main)
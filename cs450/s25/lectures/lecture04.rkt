#lang racket

;; CS 450 Spring 2025
;; Lecture 04: Tues Feb 11

;; NOTE: This code is provided as a supplement to lecture.
;; It may contain errors and may not be complete.
;; It is not to be used as "starter code" or any other part of any HW assignment.
;; All HW code must be developed from scratch according to the Design Recipe.
;; Submitting or copying any portion of the code as your own will result in zero grade.

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
;; Represents: center y coordinate "falling" circle big-bang animation
(define (WorldState? x) (nonnegative-integer? x))

(define INITIAL-WORLD 0)

(define BALL-RADIUS 50)
(define BALL-X (/ WORLD-WIDTH 2))
(define BALL-IMG
  (circle BALL-RADIUS "solid" "red"))

;; render: WorldState -> Image
;; Adds BALL-IMG to a WORLD-WIDTHxWORLD-HEIGHT empty scene at coordinate (WORLD-WIDTH/2, w)
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

;(main) ; do not automatically start big-bang (inf loop will result in autograder timeout)
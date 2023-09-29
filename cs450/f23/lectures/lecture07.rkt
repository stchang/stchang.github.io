#lang racket

;; lecture 07: Wed Sept 27, cs 450 section 2

;; new concepts ----------------------------------------
;; arbitrarily large data (lists)
;; let / internal defines
;; pattern matching
;; cond of bools - collapse to boolean arithmetic (includes if)

(require 2htdp/image
         2htdp/universe
         rackunit)

;; Multi-ball animation

;; HOW TO CREATE THIS DATA DEF?
;; A WorldState is ... an unknown number of balls


;; First, single bouncing ball example, with x and y velocity

(define LEFT-EDGE 0)
(define RIGHT-EDGE 400)
(define TOP-EDGE 0)
(define BOTTOM-EDGE 400)
(define EMPTY-SCENE (empty-scene RIGHT-EDGE BOTTOM-EDGE))

(define BALL-RADIUS 40)
(define IMG
  ;(scale .1 (bitmap/file "wwwsrc/imgs/racket.png"))
  (circle BALL-RADIUS "solid" "blue"))

;(define IMG-WIDTH (image-width IMG))
;(define IMG-WIDTH/2 (/ IMAGE-WIDTH 2))
;(define IMG-HEIGHT (image-height IMG))
;(define IMG-HEIGHT/2 (/ IMAGE-HEIGHT 2))

;; An XCoordinate is a real number in one of these intervals:
;; [LEFT-EDGE + BALL-RADIUS RIGHT-EDGE - BALL-RADIUS] : IMG fully within L/R scene
;; (-inf, LEFT-EDGE + BALL-RADIUS)         : part of IMG out of scene to the left
;; (RIGHT-EDGE - BALL-RADIUS, +inf)        : part of IMG out of scene to the right
;; Interp: The coordinate is the x coordinate of IMG center;
;; the intervals represent whether the IMG is fully within the left/right edges


;; TEMPLATE
#;(define (x-fn x)
    (cond [(<= (+ LEFT-EDGE BALL-RADIUS)
               x
               (- RIGHT-EDGE BALL-RADIUS)) ....]
          [(< x (+ LEFT-EDGE BALL-RADIUS)) ....]
          [(> x (- RIGHT-EDGE BALL-RADIUS)) ....]))
  
;; ball-in-scene/x? : XCoordinate -> Bool
;; returns true if ball is comletely in the scene, given x coord of center
(define (ball-in-scene/x? x)
  ;; template-based version
  #;(cond [(<= (+ LEFT-EDGE BALL-RADIUS)
              x
              (- RIGHT-EDGE BALL-RADIUS)) #t]
          [(< x (+ LEFT-EDGE BALL-RADIUS)) #f]
          [(> x (- RIGHT-EDGE BALL-RADIUS)) #f])
  ;; collapsed to boolean arithmetic
  (<= (+ LEFT-EDGE BALL-RADIUS) x (- RIGHT-EDGE BALL-RADIUS)))

(check-false (ball-in-scene/x? RIGHT-EDGE))
(check-false (ball-in-scene/x? LEFT-EDGE))
(check-true (ball-in-scene/x? (- RIGHT-EDGE BALL-RADIUS)))
(check-true (ball-in-scene/x? (+ LEFT-EDGE BALL-RADIUS)))
(check-true (ball-in-scene/x? (add1 (+ LEFT-EDGE BALL-RADIUS))))
(check-false (ball-in-scene/x? (sub1 (+ LEFT-EDGE BALL-RADIUS))))

;; assume same data def for YCoordinate ...
(define (ball-in-scene/y? y)
  (<= (+ TOP-EDGE BALL-RADIUS) y (- BOTTOM-EDGE BALL-RADIUS)))


;; A WorldState is a 
(struct world [x y xvel yvel] #:transparent)
;; where
;; x: XCoord - represents x coordinate of ball center in animation
;; y: YCoord - represents y coordinate of ball center in animation
;; xvel: Integer - represents x velocity, where
;;                 postive = to the right, negative = to the left
;; yvel: Integer - represents y vel, where
;;                 positive = down, negative = up


;; TEMPATE

;; world-fn : WorldState -> ???
#;(define (world-fn w)
  .... (world-x w) (world-y w) (world-xvel w) (world-yvel w) ....)

;; render-world : WorldState -> Image
;; Draws the given worldstate as an image
(define (render-world w)
  (place-image IMG (world-x w) (world-y w) EMPTY-SCENE))

;; next-world : WorldState -> WorldState
;; Computes the next worldstate from the given one
(define (next-world w)
  (match-define (world x y xvel yvel) w)
  (define new-xvel
    (if (ball-in-scene/x? x) xvel (- xvel)))
  (define new-yvel
    (if (ball-in-scene/y? y) yvel (- yvel)))
  (define new-x (+ x new-xvel))
  (define new-y (+ y new-yvel))
  (world new-x new-y new-xvel new-yvel))

;; random-x : XCoord
;; Returns a random x coordinate where IMG is fully within L/R edges
(define (random-x)
  (+ (random (- RIGHT-EDGE BALL-RADIUS)) BALL-RADIUS))

;; random-y : YCoord
;; Returns a random y coordinate where IMG is fully within top/bot edges
(define (random-y)
  (+ (random (- BOTTOM-EDGE BALL-RADIUS)) BALL-RADIUS))

(define MAX-VELOCITY 10)
(define (random-velocity) (random MAX-VELOCITY))

;; random-world : -> WorldState
;; returns a random worldstate
(define (random-world)
  (world (random-x) (random-y) (random-velocity) (random-velocity)))

(define (main)
  (big-bang (random-world)
    #;[on-mouse mouse-handler]
    [on-tick next-world]
    [to-draw render-world]))

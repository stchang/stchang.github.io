#lang racket

;; lecture 06: Monday Sept 25, cs 450 section 2

;; compound data

(require 2htdp/image
         2htdp/universe)

;; Itemizations Clarification ----------------------------------------

;; A MaybeInt is one of:
(define NaN "Not a Number")
;; or, Integer
;; Interp: represents a number with a possible error case

(define (NaN? x)
  (string=? x "Not a Number"))

;; WRONG predicate for MaybeInt
#;(define (MaybeInt? x)
  (or (NaN? x)
      (integer? x)))

;; OK predicate for MaybeInt
(define (MaybeInt? x)
  (or (and (string? x) (NaN? x))
      (integer? x)))

; WRONG TEMPLATE for MaybeInt
#;(define (maybeint-fn x)
  (cond
    [(NaN? x) ....]
    [(integer? x) ....]))

; OK TEMPLATE for MaybeInt
#;(define (maybeint-fn x)
  (cond
    l; string? is ok bc we only need to distinguish valid cases
    [(string? x) ....] 
    [(integer? x) ....]))

;; ------------------------------------------------------------
;; new concepts
;; - compound data definitions
;; - posn pre-defined data def
;; - struct Racket data structure

;; HOW TO CREATE THIS DATA DEF?
;; A WorldState is an Integer ...
;; ... and another Integer???

;; A WorldState is a
(struct world [x y] #:transparent) ; transparent structs print field values
;; where
;; x: Integer - represents x coordinate of ball/mouse in animation
;; y: Integer - represents y coordinate of ball/mouse

(define WORLD-HEIGHT 400)
(define WORLD-WIDTH 400)
(define INITIAL-WORLD (world (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)))
(define EMPTY-SCENE (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define SPRITE
  ;(scale .2 (bitmap/file "imgs/racket.png"))
  (circle 100 "solid" "blue"))

;; TEMPLATE for world-fn: WorldState -> ???
#;(define (world-fn w)
  .... (world-x w) ....
  .... (world-y w) ....)

;; TODO: fill-in missing function design recipe steps
(define (mouse-handler w x y evt)
  (world x y))

;; TODO: fill-in missing function design recipe steps
(define (render-world w)
  (place-image SPRITE (world-x w) (world-y w) EMPTY-SCENE))

(define (main)
  (big-bang INITIAL-WORLD
    [on-mouse mouse-handler]
    [to-draw render-world]))

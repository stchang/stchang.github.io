#lang racket

;; CS 450 section 2
;; lecture 06: Monday Sept 23

;; compound data

(require 2htdp/image
         2htdp/universe
         rackunit)

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
;; (MaybeInt? 1) => error (should be true)

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
    ;; string? is ok bc we only need to distinguish valid cases
    [(string? x) ....] 
    [(integer? x) ....]))

;; better data def? (put more common cases first)
;; A MaybeInt2 is one of:
;; - Integer
;; - NaN
;; Interp: represents a number with a possible error case

;; better(?) predicate for MaybeInt
(define (MaybeInt2? x)
  (or (integer? x)
      (and (string? x) (NaN? x))))

;; better(?) TEMPLATE for MaybeInt
#;(define (maybeint2-fn x)
  (cond
    [(integer? x) ....]
    [else ....]))

;; ------------------------------------------------------------
;; new concepts
;; - compound data definitions
;; - struct Racket data structure


;; IN-CLASS exercise: big-bang program with mousehandler
;; - "ball" follows mouse cursor

;; HOW TO CREATE THIS DATA DEF?
;; A WorldState is an Integer ...
;; ... and another Integer???

;; A Coordinate is an Integer
;; interp: represents one axis in a big-bang coordinate
(define (Coordinate? x) (integer? x))

;; A WorldState is a (make-world [x : Coordinate] [y : Coordinate])
;; where
;; x : represents center x coordinate of ball/mouse in animation
;; y : represents center y coordinate of ball/mouse

;; NOTE: #:transparent structs:
;; - print field values
;; - and can be compared with equal?, check-equal?, etc
(struct world [x y] #:transparent) 
(define/contract (make-world x y)
  (-> Coordinate? Coordinate? world?)
  (world x y))
(define WORLD-HEIGHT 400)
(define WORLD-WIDTH 400)
(define EMPTY-SCENE
  (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define INITIAL-WORLD
  (make-world (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)))

(define SPRITE
  (circle 100 "solid" "blue"))

;; TEMPLATE for world-fn: WorldState -> ???
#;(define (world-fn w)
  .... (world-x w) ....
  .... (world-y w) ....)

;; mouse-handler : WorldState Coordinate Coordinate MouseEvent -> WorldState
;; Sets the WorldState to be the current mouse location
(define/contract (mouse-handler w x y evt)
  (-> world? Coordinate? Coordinate? mouse-event? world?)
  (make-world x y))

;; ignore mouse clicks and set ball coord to mouse coord
(check-equal? (mouse-handler (world 1 1) 0 0 "button-down")
              (world 0 0))

;; TODO: fill-in missing function design recipe steps
;; NOTICE: how this function follows the world-fn TEMPLATE
(define/contract (render-world w)
  (-> world? image?)
  (place-image SPRITE (world-x w) (world-y w) EMPTY-SCENE))

(define (main)
  (big-bang INITIAL-WORLD
    [on-mouse mouse-handler]
    [to-draw render-world]))

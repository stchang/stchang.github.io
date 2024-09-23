#lang racket

;; CS 450 section 2, lecture 05, Wed Sep 18, 2024

(require (for-syntax syntax/parse)
         rackunit
         2htdp/image
         2htdp/universe)

(define-syntax .... ; placeholder
  (syntax-parser [:id #'(void)]))

;; ------------------------------------------------------------
;; Interval data definitions

;; An AngleD is a number in [0, 360)
;; interp: An angle in degrees
(define (AngleD? deg)
  (and (>= deg 0) (< deg 360)))

;; An AngleR is a number in [0 2π)
;; interp: An angle in radians
(define (AngleR? r)
  (and (>= r 0) (< r (* 2 pi))))

;; deg->rad: AngleD -> AngleR
;; Converts the given angle in degrees to radians
(define/contract (deg->rad deg)
  (-> AngleD? AngleR?)
  (* deg (/ pi 180)))
(check-equal? (deg->rad 0) 0)
(check-equal? (deg->rad 90) (/ pi 2))
(check-equal? (deg->rad 180) pi)

;; what should be the result of these??
;;(check-equal? (deg->rad 360) 0) ; ???
;;(check-equal? (deg->rad 360) (* 2 pi)) ; ???

;; ------------------------------------------------------------
;; Enumeration Data Definitions

;; Traffic Light Representation #1
;; NOTE: see TrafficLight2 below for comparison of two different data defs

;; A TrafficLightState is one of:
(define RED-LIGHT "RED")
(define GREEN-LIGHT "GREEN")
(define YELLOW-LIGHT "YELLOW")
;; Interpretation: Represents possible colors of a traffic light
(define (red-light? x) (string=? x RED-LIGHT))
(define (green-light? x) (string=? x GREEN-LIGHT))
(define (yellow-light? x) (string=? x YELLOW-LIGHT))

(define (TrafficLightState? x)
  (or (red-light? x)
      (green-light? x)
      (yellow-light? x)))

;; next-light: TrafficLightState -> TrafficLightState
;; Computes the next light after the given one
(define/contract (next-light light)
  (-> TrafficLightState? TrafficLightState?)
  (cond
    [(red-light? light) GREEN-LIGHT]
    [(green-light? light) YELLOW-LIGHT]
    [(yellow-light? light) RED-LIGHT]))
  
(check-equal? (next-light RED-LIGHT) GREEN-LIGHT)
(check-equal? (next-light GREEN-LIGHT) YELLOW-LIGHT)
(check-equal? (next-light YELLOW-LIGHT) RED-LIGHT)

(define FRAME-WIDTH 240)
(define FRAME-HEIGHT 200)
(define LIGHT-FRAME
  (rectangle FRAME-WIDTH FRAME-HEIGHT "solid" "black"))

(define FONT-SIZE 48) ; pt

;; light->textimg: TrafficLightState -> Image
;; Constructs a text image of the traffic light state
(define/contract (light->textimg li)
  (-> TrafficLightState? image?)
  (text li FONT-SIZE li))

;; render-light: TrafficLightState -> Image
;; Constructs an image of the trafficlight in the given state
(define/contract (render-light li)
  (-> TrafficLightState? image?)
  (overlay (light->textimg li) LIGHT-FRAME))

;; click?: MouseEvt -> Boolean
;; predicate for "button-down" mouse event
(define (click? evt)
  (string=? evt "button-down"))

;; handle-click: TrafficLightState Coordinate Coordinate MouseEvent
;; changes traffic light on mouse click
(define/contract (handle-click li x y evt)
  (-> TrafficLightState? integer? integer? mouse-event?
      TrafficLightState?)
  (cond
    [(click? evt) (next-light li)]
    [else li]))

;; run: -> TrafficLightState
;; runs the traffic light simulation
;; NOTE: see also run2 below for comparison of two different data defs
(define (run)
  (big-bang RED-LIGHT
    [to-draw render-light]
    [on-mouse handle-click]))

;; ------------------------------------------------------------
;; Itemization data

;; A Salary is one of:
;; [0, 11000)
;; [11000 44725)
;; [44725, 95375)
;; ...
;; Interp: yearly salary in US Dollars,
;;         split by 2024 tax bracket
(define (10%-bracket? salary)
  (and (>= salary 0)
       (< salary 11000)))
(define (12%-bracket? salary)
  (and (>= salary 11000)
       (< salary 44725)))
;; ...

;; taxes-owed: Salary -> USD
;; computes federal income tax owed in 2024
(define (taxes-owed salary)
  (cond
    [(10%-bracket? salary) ....]
    [(12%-bracket? salary) ....]
    [else ....]))


;; ------------------------------------------------------------
;; traffic light animation, with alternate data def (TLState2)

;; NOTE: to make it easier to distinguish and compare with the
;; first simulation above, all these names have the "2" suffix
;; even though that's not a great naming choice

;; A TLState2 is one of:
(define GREEN-L 0)
(define YELLOW-L 1)
(define RED-L 2)
;; Interp: represents a traffic light state
(define (red-L? li) (= li RED-L))
(define (green-L? li) (= li GREEN-L))
(define (yellow-L? li) (= li YELLOW-L))

(define START-L GREEN-L)

(define TLState2? (disjoin red-L? green-L? yellow-L?))

;; light2->color: TLState2 -> Color
;; converts traffic light state to a color
;; NOTE: this was not needed above
(define/contract (light2->color li)
  (-> TLState2? image-color?)
  (cond
    [(red-L? li) "red"]
    [(green-L? li) "green"]
    [(yellow-L? li) "yellow"]))

(define/contract (light2->textimg li)
  (-> TLState2? image?)
  (text (light2->color li) FONT-SIZE (light2->color li)))

;; constants for rendering an actual traffic light img
(define LIGHT-RADIUS 40)
(define LIGHT-HEIGHT (* 2 LIGHT-RADIUS))
(define LIGHT-SEP LIGHT-RADIUS)

;; these constants are same as above
#;(define FRAME-WIDTH2 160)
#;(define FRAME-HEIGHT2 400)
#;(define LIGHT2-FRAME
  (rectangle FRAME-WIDTH2 FRAME-HEIGHT2 "solid" "black"))
;; render-light2: TLState2 -> Image
(define/contract (render-light2 li)
  (-> TLState2? image?)
  (overlay (light2->textimg li) LIGHT-FRAME))

(define NUM-LIGHTS 3)
;; next-light2: TLState2 -> TLState2
;; computes next traffic light state
;; NOTE: this is easier than with TrafficLight above
(define/contract (next-light2 li)
  (-> TLState2? TLState2?)
  (modulo (add1 li) NUM-LIGHTS))

;; handle-mouse: TLState2 Coordinate Coordinate MouseEvent
;; Handles mouse event for traffic light simulation
(define/contract (handle-mouse2 li x y evt)
  (-> TLState2? integer? integer? mouse-event? TLState2?)
  (cond
    [(click? evt) (next-light2 li)]
    [else li]))

;; run2: -> TLState2
;; compare with `run` above for first TrafficLight data def
(define (run2)
  (big-bang START-L
    [on-mouse handle-mouse2]
    [to-draw render-light2]))

#lang racket

;; CS 450, lecture 05, Wed Sep 20, 2023

(require (for-syntax syntax/parse)
         rackunit)

(define-syntax ....
  (syntax-parser
    [:id #'(void)]))

;; Interval data definitions --------------------

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

;; Enumeration Data Definitions

;; A TrafficLight is one of:
(define RED-LIGHT "RED")
(define GREEN-LIGHT "GREEN")
(define YELLOW-LIGHT "YELLOW")
;; Interpretation: Represents possible colors of a traffic light
(define (red-light? x) (string=? x RED-LIGHT))
(define (green-light? x) (string=? x GREEN-LIGHT))
(define (yellow-light? x) (string=? x YELLOW-LIGHT))

(define (TrafficLight? x)
  (or (red-light? x)
      (green-light? x)
      (yellow-light? x)))

;; next-light: TrafficLight -> TrafficLight
;; Computes the next light after the given one
(define (next-light light)
  (cond
    [(red-light? light) GREEN-LIGHT]
    [(green-light? light) YELLOW-LIGHT]
    [(yellow-light? light) RED-LIGHT]))
  
(check-equal? (next-light RED-LIGHT) GREEN-LIGHT)
(check-equal? (next-light GREEN-LIGHT) YELLOW-LIGHT)
(check-equal? (next-light YELLOW-LIGHT) RED-LIGHT)

(define FRAME-WIDTH 160)
(define FRAME-HEIGHT 400)
(define LIGHT-FRAME
  (rectangle FRAME-WIDTH FRAME-HEIGHT "solid" "black"))

(define FONT-SIZE 48) ; pt

(define (light->textimg li)
  (text li FONT-SIZE li))
  
(define (render-light li)
  (overlay (light->textimg li) LIGHT-FRAME))

(define (click? evt)
  (string=? evt "button-down"))

;; handle-click: TrafficLight Coordinate Coordinate MouseEvent
;; changes traffic light on mouse click
(define (handle-click li x y evt)
  (cond
    [(click? evt) (next-light li)]
    [else li]))

(define (run1)
  (big-bang RED-LIGHT
    [to-draw render-light]
    [on-mouse handle-click]))
  
;; A Salary is one of:
;; [0, 11000)
;; [11000 44725)
;; [44725, 95375)
;; ...
;; Interp: yearly salary in US Dollars,
;;         split by 2023 tax bracket
(define (10%-bracket? salary)
  (and (>= salary 0)
       (< salary 11000)))
(define (12%-bracket? salary)
  (and (>= salary 11000)
       (< salary 44725)))
;; ...

;; taxes-owed: Salary -> TaxBalance
;; computes federal income tax owed in 2023
(define (taxes-owed salary)
  (cond
    [(10%-bracket? salary) ....]
    [(12%-bracket? salary) ....]
    [else ....]))


;; ------------------------------------------------------------
;; traffic light animation
(require 2htdp/image
         2htdp/universe)

;; A TrafficLight2 is one of:
(define GREEN-L 0)
(define YELLOW-L 1)
(define RED-L 2)
;; Interp: represents a traffic light state
(define (red-L? li) (= li RED-L))
(define (green-L? li) (= li GREEN-L))
(define (yellow-L? li) (= li YELLOW-L))

(define NUM-LIGHTS 3)
(define START-L GREEN-L)

(define LIGHT-RADIUS 40)
(define LIGHT-HEIGHT (* 2 LIGHT-RADIUS))

(define (light->color li)
  (cond
    [(red-L? li) "red"]
    [(green-L? li) "green"]
    [(yellow-L? li) "yellow"]))

(define LIGHT-SEP LIGHT-RADIUS)

(define (light2->textimg li)
  (text (light->color li) FONT-SIZE (light->color li)))

;; render-light2: TrafficLight2 -> Image
(define (render-light2 li)
  (overlay (light2->textimg li) LIGHT-FRAME)
  #;(overlay/xy
   (circle LIGHT-RADIUS "solid" (light->color li))
   (- LIGHT-SEP)   ; shift bot img left = neg offset
   (- (* (add1 li) ; shift bot img down = neg offset
         (+ LIGHT-HEIGHT LIGHT-SEP))
      FRAME-HEIGHT)
   LIGHT-FRAME))

(define (next-light2 li)
  (modulo (add1 li) NUM-LIGHTS))

;; handle-mouse: TrafficLight2 Coordinate Coordinate MouseEvent
;; Handles mouse event for traffic light simulation
(define (handle-mouse2 li x y evt)
  (cond
    [(click? evt) (next-light2 li)]
    [else li]))

(define (run2)
  (big-bang START-L
    [on-mouse handle-mouse2]
    [to-draw render-light2]))

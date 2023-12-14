#lang racket

;; cs450, fall 2023, section 2
;; 12/11/2023 lecture 27

;; How to design ... OO programs?

;; The two goals of this lecture is to show:
;; 1. The lessons learned in this course still apply with OOP
;; 2. There's nothing special about OOP!
;;    It's just a specific choice of data design!

;; Specifically, it's an itemization of compound data  ...
;; ... that groups together the (class) fields of the data,
;; with functions (i.e., "methods") that process that data

;; The differences are:
;; - in an OO lang, compound data (defined with a "class") typically
;;   groups non-function and functions (i.e., "methods") together,
;;   where the function are additional (hidden) fields.
;; - a class constructor implicitly populates the method fields for you
;; - in an OO lang, the "dispatch method" is implicitly defined for you

;; We'll demonstrate this with an example.

(require 2htdp/image
         rackunit)

;; A Shape is one of:
;; - Circle
;; - Rectangle
;; interp: represents a shape image
;; Required methods:
;; render : Shape -> Image
(struct Shape [render])

;; render : Shape -> Image
;; "dispatch" function for Shape values
(define (render sh)
  ((Shape-render sh) sh))

;; A Circle is a Shape constructed with:
;;   (circ render : Circle -> Image
;;              r : nonnegative-number
;;            col : Color)
;; interp:
;; - render is a function that converts the circ into a (2htdp/image) Image
;; - r is the circle radius
;; - col is the (2htdp/image) Color
(struct circ Shape [r col])

;; new-circ : nonnegative-number Color [Circle -> Image] -> Circle
;; Alternative constructor where the render method is implicit
(define (new-circ r col [render render-circ])
  (circ render r col))

;; render-circ : Circle -> Image
;; Converts the given converts the circ into a (2htdp/image) Image
(define (render-circ c)
  (circle (circ-r c) "solid" (circ-col c)))

;; A Rectangle is a Shape constructed with:
;;   (rect render : Rectangle -> Image
;;              w : nonnegative-number
;;              h : nonnegative-number
;;            col : Color)
;; interp:
;; - render is a function that converts the rect into a (2htdp/image) Image
;; - w and h are the width and height, respectively
;; - col is the (2htdp/image) Color
(struct rect Shape [w h col])

;; new-rect : nonneg-num nonneg-num Color [Rectangle -> Image] -> Rectanglex
;; Alternative constructor where the render method is implicit
(define (new-rect w h col [render render-rect])
  (rect render w h col))

;; render-rect : rectle -> Image
;; Converts the given converts the rect into a (2htdp/image) Image
(define (render-rect r)
  (rectangle (rect-w r) (rect-h r) "solid" (rect-col r)))


(let ([r (new-rect 10 20 "blue")])
  (check-equal?
   (render r)
   (render-rect r)
   "test correct method being called")
  (check-equal?
   (render r)
   (rectangle 10 20 "solid" "blue")
   "test correct render rect output"))

(let ([c (new-circ 10 "red")])
  (check-equal?
   (render c)
   (render-circ c)
   "test correct method being called")
  (check-equal?
   (render c)
   (circle 10 "solid" "red")
   "test correct render circ output"))

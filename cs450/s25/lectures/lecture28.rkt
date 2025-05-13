#lang racket

(require 2htdp/image)

;; CS 450, Spring 2025
;; 5/13/2025, Lecture 28

;; mixins demo

;; NOTE:
;; This code is a demo to demonstrate how a language
;; with mixins and class Results may be used to extend
;; an existing class with a new method without modifying it.


(define Shape<%>
 (interface ()
   [render
    (recursive-contract
     (-> (is-a?/c Shape<%>) image?))]))

(define Rectangle%
  (class* object% (Shape<%>)
    (super-new)

    (init-field width height color)
    
    (define/public (render)
      (rectangle width height "solid" color))))

(send
 (new Rectangle% [width 100] [height 200] [color "blue"])
 render)

(define Circle%
  (class* object% (Shape<%>)
    (super-new)
    
    (init-field radius color)

    (define/public (render)
      (circle radius "solid" color))))

(send
 (new Circle% [radius 50] [color "red"])
 render)

;; adds a "render-outline" method to an existing class
(define (circle-outline-mixin existing-class)
  (class existing-class
    (super-new)
    (inherit-field radius color)
    (define/public (render-outline)
      (circle radius "outline" color))))

(send
 (new (circle-outline-mixin Circle%) [radius 50] [color "red"])
 render-outline)
                       
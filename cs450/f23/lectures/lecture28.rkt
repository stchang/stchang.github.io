#lang racket

(require 2htdp/image)

;; cs450, fall 2023, section 2
;; 12/13/2023 lecture 28

;; mixins demo

;; NOTE:
;; This code is strictly a demo to demonstrate how a language
;; with mixins and first-class classes may be used to extend
;; an existing class with a new method without modifying it.
;; 
;; Interfaces and classes are not to be used in hw10.
;; (See lecture27 for more details about how to write in the
;; "OO-style" needed for hw10)

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
                       
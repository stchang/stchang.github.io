  #lang racket

;; lecture 08: mon 10/2, cs 450 section 2

;; new concepts ----------------------------------------
;; arbitrarily large data (lists)
;; recursive data definitions

(require 2htdp/image
         2htdp/universe
         rackunit)

;; Multi-ball animation

(define LEFT-EDGE 0)
(define RIGHT-EDGE 400)
(define TOP-EDGE 0)
(define BOTTOM-EDGE 400)
(define EMPTY-SCENE (empty-scene RIGHT-EDGE BOTTOM-EDGE))

(define BALL-RADIUS 40)
(define IMG
  (circle BALL-RADIUS "solid" "blue"))

;; An XCoordinate is a real number in one of these intervals:
;; [LEFT-EDGE + BALL-RADIUS,  RIGHT-EDGE - BALL-RADIUS] : IMG fully within L/R scene
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
  ;; cond that evaluates to bool can be collapsed to boolean arithmetic
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


;; A ListofBall is one of
;; - empty
;; - (cons Ball ListofBall)

;; A WorldState is a ListofBall

;; A Ball is a 
(struct ball [x y xvel yvel] #:transparent)
;; where
;; x: XCoord - represents x coordinate of ball center in animation
;; y: YCoord - represents y coordinate of ball center in animation
;; xvel: Integer - represents x velocity, where
;;                 postive = to the right, negative = to the left
;; yvel: Integer - represents y vel, where
;;                 positive = down, negative = up


;; TEMPATE

;; world-fn : WorldState -> ???
;; (compound data template extracts pieces)
#;(define (world-fn w)
  .... (world-x w) (world-y w) (world-xvel w) (world-yvel w) ....)

;; render-world : WorldState -> Image
;; Draws the given worldstate as an image
(define (render-world w)
  (cond
    [(empty? w) EMPTY-SCENE]
    [else (place-ball (first w) (render-world (rest w)))]))

(define (place-ball b scene)
  (place-image IMG (ball-x b) (ball-y b) scene))

;; next-world : WorldState -> WorldState
;; Computes the next worldstate from the given one
(define (next-world w)
  #;(cond ; template
    [(empty? w) ....]
    [else .... (first w) ...
          (next-world (rest w)) ....])
  (cond
    [(empty? w) empty]
    [else (cons (next-ball (first w))
                (next-world (rest w)))]))

;; next-ball : Ball -> Ball
;; Computes a ball with position updated after one tick
;; (this was the next-world fn, with only one ball)
(define (next-ball b)
  (match-define (ball x y xvel yvel) b)
  (define new-xvel
    (if (ball-in-scene/x? x) xvel (- xvel)))
  (define new-yvel
    (if (ball-in-scene/y? y) yvel (- yvel)))
  (define new-x (+ x new-xvel))
  (define new-y (+ y new-yvel))
  (ball new-x new-y new-xvel new-yvel))

(check-equal? (next-world (list (ball 0 0 1 1)))
              (list (next-ball (ball 0 0 1 1))))

;; random-x : XCoord
;; Returns a random x coordinate where IMG is fully within L/R edges
(define (random-x)
  (random (+ LEFT-EDGE BALL-RADIUS)
          (- RIGHT-EDGE BALL-RADIUS)))

;; random-y : YCoord
;; Returns a random y coordinate where IMG is fully within top/bot edges
(define (random-y)
  (random (+ LEFT-EDGE BALL-RADIUS)
          (- RIGHT-EDGE BALL-RADIUS)))

(define MAX-VELOCITY 10)
(define (random-velocity) (random MAX-VELOCITY))

(define (make-ball/random-velocity x y)
  (ball x y (random-velocity) (random-velocity)))

;; random-ball : -> Ball
;; returns a ball with random pos in the scene, and velocity
(define (random-ball)
  (make-ball/random-velocity (random-x) (random-y)))

;; click : MouseEvent -> Boolean
;; returns true if mevt is "button-down"
(define (click? mevt)
  (string=? mevt "button-down"))

;; mouseHandler : WorldState XCoord YCoord MouseEvent -> WorldState
;; Inserts a new ball on mouse click, at given mouse x and y
;; NOTE: this function has a bug (try clicking close to edge)
(define (mouse-handler w x y mevt)
  (cond
    [(click? mevt) (cons (make-ball/random-velocity x y) w)]
    [else w]))

(define (main)
  (big-bang (list (random-ball))
    [on-mouse mouse-handler]
    [on-tick next-world]
    [to-draw render-world]))

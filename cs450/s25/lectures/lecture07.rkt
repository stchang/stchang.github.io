#lang racket

;; CS 450, spring 2025
;; lecture 07: Feb 20

;; Programming with Compound Data

(require 2htdp/image
         2htdp/universe
         rackunit)

;; Bouncing ball example, with x and y velocity

(define SCENE-WIDTH 400)
(define SCENE-HEIGHT 400)
(define EMPTY-SCENE
  (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define BALL-RADIUS 40)
(define IMG
  ;(scale .1 (bitmap/file "wwwsrc/imgs/racket.png"))
  (circle BALL-RADIUS "solid" "blue"))

(define LFT-EDGE 0)
(define RGT-EDGE SCENE-WIDTH)
(define TOP-EDGE 0)
(define BOT-EDGE SCENE-HEIGHT)
  
(define VELOCITY 10)

;; functions to convert from middle of ball to an edge, and vice versa
;; (dont need explicit data definitions, because these can be any coordinates;
;; we are not concerned specific intervals here)

;; real -> real
(define (mid->rgt x)
  (+ x BALL-RADIUS))
(define (mid->lft x)
  (- x BALL-RADIUS))
(define (lft->mid x)
  (+ x BALL-RADIUS))
(define (rgt->mid x)
  (- x BALL-RADIUS))
(define (mid->bot y)
  (+ y BALL-RADIUS))
(define (mid->top y)
  (- y BALL-RADIUS))
(define (top->mid y)
  (+ y BALL-RADIUS))
(define (bot->mid y)
  (- y BALL-RADIUS))

;; An AllX is a real that is either:
;; < (lft->mid LEFT-EDGE)
;; > (rgt->mid RIGHT-EDGE)
;; InSceneX
;; Represents all possible ball center x coordinates
(define (AllX? x) (real? x))

(define (past-right-edge? x)
  (> x (rgt->mid RGT-EDGE)))
(define (past-left-edge? x)
  (< x (lft->mid LFT-EDGE)))

;; TEMPLATE
#;(define (AllX-fn x)
    (cond [(past-left-edge? x) ....]
          [(past-right-edge? x) ....]
          [(InSceneX? x) ....]))
  
;; A InSceneX is a positive real in the interval
;; [(rgt->mid RIGHT-EDGE), (lft->mid LEFT-EDGE)]
;; Represents center x when ball completely in the scene
(define (InSceneX? x) (<= (lft->mid LFT-EDGE) x (rgt->mid RGT-EDGE)))

;; An AllY is a real that is either:
;; < (top->mid TOP-EDGE)
;; > (bot->mid BOT-EDGE)
;; InSceneY
;; Represents all possible ball center y coordinates
(define (AllY? y) (real? y))

(define (past-top-edge? x)
  (< x (top->mid TOP-EDGE)))
(define (past-bot-edge? x)
  (> x (bot->mid BOT-EDGE)))

;; A InSceneY is a positive real in the interval
;; [(top->mid TOP-EDGE), (bot->mid BOT-EDGE)]
;; Represents center y when ball completely in the scene
(define (InSceneY? y) (<= (top->mid TOP-EDGE) y (bot->mid BOT-EDGE)))

;; A Velocity (Vel) is int between [-10, 10]
;; Interp:
;; for x axis, postive = to the right, negative = to the left
;; for y asix, positive = down, negative = up
(define (Velocity? v)
  (<= -10 v 10))

;; A WorldState is a
;; (make-world [x : InSceneX] [y : InSceneY] [xv : Vel] [yv : Vel])
(struct world [x y xvel yvel] #:transparent)
(define (WorldState? x) (world? x))
(define/contract (mk-WorldState x y xv yv)
  (-> InSceneX? InSceneY? Velocity? Velocity? WorldState?)
  (world x y xv yv))
;; where
;; x : represents x coordinate of ball center in animation
;; y : represents y coordinate of ball center in animation
;; xv : represents left-right velocity
;; yv : represents up/down velocity



;; TEMPATE

;; world-fn : WorldState -> ???
#;(define (world-fn w)
  .... (world-x w) (world-y w) (world-xvel w) (world-yvel w) ....)

;; render-world : WorldState -> Image
;; Draws the given worldstate as an image
(define/contract (render-world w)
  (-> WorldState? image?)
  (place-image IMG (world-x w) (world-y w) EMPTY-SCENE))

;; next-world : WorldState -> WorldState
;; Computes the next worldstate from the given one
(define/contract (next-world w)
  (-> WorldState? WorldState?)
  (match-define (world x y xv yv) w)
  (define new-xv (next-xv x xv))
  (define new-yv (next-yv y yv))
  (define new-x (next-x x xv))
  (define new-y (next-y y yv))
  (mk-WorldState new-x new-y new-xv new-yv))

(define/contract (next-x x xv)
  (-> InSceneX? Velocity? InSceneX?)
  (x->in-scene-x (+ x xv)))

(define/contract (x->in-scene-x new-x)
  (-> AllX? InSceneX?)
  (cond
    [(InSceneX? new-x) new-x]
    [(past-right-edge? new-x) (rgt->mid RGT-EDGE)]
    [(past-left-edge? new-x) (lft->mid LFT-EDGE)]))


(define/contract (next-y y v)
  (-> InSceneY? Velocity? InSceneY?)
  (y->in-scene-y (+ y v)))
(define/contract (y->in-scene-y new-y)
  (-> AllY? InSceneY?)
  (cond
    [(InSceneY? new-y) new-y]
    [(past-top-edge? new-y) (top->mid TOP-EDGE)]
    [(past-bot-edge? new-y) (bot->mid BOT-EDGE)]))

(define/contract (next-xv x xv)
  (-> InSceneX? Velocity? Velocity?)
  (update-vel/x xv (+ x xv))) ; may be reasonable to combine these two fns?
;; reverses given vel if given x coordinate is not in scene
(define/contract (update-vel/x v x)
  (-> Velocity? AllX? Velocity?)
  (cond
    [(InSceneX? x) v]
    [else (rev v)]))

(define/contract (next-yv y yv)
  (-> InSceneY? Velocity? Velocity?)
  (update-vel/y yv (+ y yv))) ; may be reasonable to combine these two fns?
;; reverses given vel if given y coordinate is not in scene
(define/contract (update-vel/y v y)
  (-> Velocity? AllY? Velocity?)
  (cond
    [(InSceneY? y) v]
    [else (rev v)]))

;; changes direction of given velocity 180 degrees
(define/contract (rev v)
  (-> Velocity? Velocity?)
  (- v))

;; random-x : -> InSceneX
;; Returns a random x coordinate where IMG is fully within L/R edges
(define (random-x)
  (random (lft->mid LFT-EDGE) (rgt->mid RGT-EDGE)))

;; random-y : -> InSceneY
;; Returns a random y coordinate where IMG is fully within top/bot edges
(define (random-y)
  (random (top->mid TOP-EDGE) (bot->mid BOT-EDGE)))

(define MAX-VELOCITY 10)
(define (random-velocity) (random MAX-VELOCITY))

;; random-world : -> WorldState
;; returns a random worldstate
(define (random-world)
  (world (random-x) (random-y) (random-velocity) (random-velocity)))

(define (main)
  (big-bang (random-world)
    [on-tick next-world]
    [to-draw render-world]))

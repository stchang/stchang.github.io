#lang racket

;; cs 450 spring 2025
;; lecture 08: Tues 2/25 - Recursive Data Definitions

(require 2htdp/image
         2htdp/universe
         rackunit)

;; new concepts ----------------------------------------
;; arbitrarily large data (lists)
;; recursive data definitions

;; A ListofInts is one of:
;; - empty
;; - (cons Int ListofInts)

;; TEMPLATE for ListofInts
;; (start with this when writing list-processing functions;
;;  do not submit this)
#;(define (list-fn lst)
    (cond
      [(empty? lst) ....]
      [else
       .... (first lst) ....
       .... (list-fn (rest lst)) ....]))

(define/contract (sum-lst lst)
  (-> (listof integer?) integer?)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
             (sum-lst (rest lst)))]))

(check-equal? (sum-lst empty) 0)
(check-equal? (sum-lst (list 1 2 3 4 5)) 15)

;; Multi-ball animation

(define LEFT-EDGE 0)
(define RIGHT-EDGE 400)
(define TOP-EDGE 0)
(define BOTTOM-EDGE 400)
(define EMPTY-SCENE (empty-scene RIGHT-EDGE BOTTOM-EDGE))

(define BALL-RADIUS 40)
(define IMG
  (circle BALL-RADIUS "solid" "blue"))

;; fns to convert between Ball center and various outer points
;; That is, converts between BallCenterX and other x coordinates
(define (lft->mid x) (+ x BALL-RADIUS))
(define (mid->lft x) (- x BALL-RADIUS))
(define (rgt->mid x) (- x BALL-RADIUS))
(define (mid->rgt x) (+ x BALL-RADIUS))

(define (mid->bot y) (+ y BALL-RADIUS))
(define (bot->mid y) (- y BALL-RADIUS))
(define (mid->top y) (- y BALL-RADIUS))
(define (top->mid y) (+ y BALL-RADIUS))
  
;; An InSceneX is the interval [(lft->mid LEFT-EDGE), (rgt->mid RIGHT-EDGE)]
;; Represents: x coord of ball completely in the scene
(define (InSceneX? x)
  (<= (lft->mid LEFT-EDGE) x (rgt->mid RIGHT-EDGE)))

(define (ball-in-scene/x? x) (InSceneX? x))
(check-false (ball-in-scene/x? RIGHT-EDGE))
(check-false (ball-in-scene/x? LEFT-EDGE))
(check-true (ball-in-scene/x? (- RIGHT-EDGE BALL-RADIUS)))
(check-true (ball-in-scene/x? (+ LEFT-EDGE BALL-RADIUS)))
(check-true (ball-in-scene/x? (add1 (+ LEFT-EDGE BALL-RADIUS))))
(check-false (ball-in-scene/x? (sub1 (+ LEFT-EDGE BALL-RADIUS))))

;; OffScene/R is interval ((rgt->mid RIGHT-EDGE) +inf)
;; Represents: x coord of ball out of scene to the right
(define (OffScene/R? x) (> x (rgt->mid RIGHT-EDGE)))

;; OffScene/L: is interval (-inf, (lft->mid LEFT-EDGE))
;; Represents: x coord of ball out of scene to the left
(define (OffScene/L? x) (< x (lft->mid LEFT-EDGE)))

;; Define a similar set of data defs for Y coordinates:
(define (InSceneY? x)
  (<= (top->mid TOP-EDGE) x (bot->mid BOTTOM-EDGE)))
(define (OffScene/Bot? x) (> x (bot->mid BOTTOM-EDGE)))
(define (OffScene/Top? x) (< x (top->mid TOP-EDGE)))

;; An XCoord, for a Ball center x is a real number in one of the following
;; intervals, which are partitioned according to whether ball is in scene:
;; - InSceneX
;; - OffScene/R - part of IMG out of scene to the right
;; - OffScene/L - part of IMG out of scene to the left
;; Interp: The coordinate is the x coordinate of IMG center;
;; the intervals represent whether the IMG is fully within the left/right edges

;; TEMPLATE
#;(define (ballx-fn x)
    (cond [(InScene? x) ....]
          [(OffScene/R? x) ....]
          [(OffScene/L? x) ....]))

(define (XCoord? x) (real? x))
(define (YCoord? y) (real? y))

;; The following fns are used to adjust a potentially off scene coordinate
;; into out that is guaranteed to be fully in-scene

;; if given x coord that would place any part of ball out of scene,
;; bring ball completely into scene touching closest edge
(define/contract (adjx x)
  (-> XCoord? InSceneX?)
  (cond [(InSceneX? x) x]
        [(OffScene/L? x) (lft->mid LEFT-EDGE)]
        [(OffScene/R? x) (rgt->mid RIGHT-EDGE)]))

;; if given y coord that would place any part of ball out of scene,
;; bring ball completely into scene touching closest edge
(define/contract (adjy y)
  (-> YCoord? InSceneY?)
  (cond [(InSceneY? y) y]
        [(OffScene/Top? y) (top->mid TOP-EDGE)]
        [(OffScene/Bot? y) (bot->mid BOTTOM-EDGE)]))

(define MAX-VELOCITY 10)
;; A Velocity is an int in (-MAX-VELOCITY, MAX-VELOCITY)
;; sign indicates direction where + is down (for y) and to the right (for x)

(define (Velocity? v) (< (- MAX-VELOCITY) v MAX-VELOCITY))
(define/contract (random-velocity)
  (-> Velocity?)
  (random MAX-VELOCITY))

;; A ListofBall is one of
;; - empty
;; - (cons Ball ListofBall)

;; A WorldState is a ListofBall
;; World state is list, so WorldState template is list fn

;; A Ball is a (mk-Ball [x : InSceneX] [y : InSceneY] [xv : Velocity] [yv : Velocity])
(struct ball [x y xvel yvel] #:transparent)
;; where
;; x: represents x coordinate of ball center in animation
;; y: represents y coordinate of ball center in animation
;; xvel: Integer - represents x velocity, where
;;                 postive = to the right, negative = to the left
;; yvel: Integer - represents y vel, where
;;                 positive = down, negative = up
(define/contract (mk-Ball x y xv yv)
  (-> InSceneX? InSceneY? Velocity? Velocity? ball?)
  (ball x y xv yv))

;; TEMPATE

;; Ball-fn : Ball -> ???
;; (compound data template extracts pieces)
#;(define (Ball-fn b)
  .... (ball-x b) (ball-y b) (ball-xv b) (ball-yv b) ....)

;; World->Image : WorldState -> Image
;; Draws the given worldstate as an image
(define/contract (World->Image w)
  (-> (listof ball?) image?)
  (cond
    [(empty? w) EMPTY-SCENE]
    [else (place-ball (first w) (World->Image (rest w)))]))

(define (place-ball b scene)
  (place-image IMG (ball-x b) (ball-y b) scene))

;; next-World : WorldState -> WorldState
;; Computes the next worldstate from the given one
(define (next-World w)
  #;(cond ; template
    [(empty? w) ....]
    [else .... (first w) ...
          (next-World (rest w)) ....])
  (cond
    [(empty? w) empty]
    [else (cons (next-Ball (first w))
                (next-World (rest w)))]))

;; reverses velocity if given coordinate is off scene
(define/contract (update-xvel x xv)
  (-> XCoord? Velocity? Velocity?)
  (cond [(InSceneX? x) xv]
        [(OffScene/R? x) (- xv)]
        [(OffScene/L? x) (- xv)]))

(define/contract (update-yvel y yv)
  (-> YCoord? Velocity? Velocity?)
  (cond [(InSceneY? y) yv]
        [(OffScene/Top? y) (- yv)]
        [(OffScene/Bot? y) (- yv)]))

;; next-Ball : Ball -> Ball
;; Computes a ball with position updated after one tick
;; (this was the next-world fn, with only one ball)
(define/contract (next-Ball b)
  (-> ball? ball?)
  (match-define (ball x y xv yv) b)
  (mk-Ball
   (adjx (+ x xv))
   (adjy (+ y yv))
   (update-xvel (+ x xv) xv)
   (update-yvel (+ y yv) yv)))

(check-equal? (next-World (list (ball 0 0 1 1)))
              (list (next-Ball (ball 0 0 1 1))))

;; random-x : XCoord
;; Returns a random x coordinate where IMG is fully within L/R edges
(define/contract (random-x) (-> InSceneX?)
  (random (lft->mid LEFT-EDGE) (rgt->mid RIGHT-EDGE)))

;; random-y : YCoord
;; Returns a random y coordinate where IMG is fully within top/bot edges
(define/contract (random-y) (-> InSceneY?)
  (random (top->mid TOP-EDGE) (bot->mid BOTTOM-EDGE)))

(define/contract (make-ball/random-velocity x y)
  (-> InSceneX? InSceneY? ball?)
  (ball x y (random-velocity) (random-velocity)))

;; random-ball : -> Ball
;; returns a ball with random pos in the scene, and velocity
(define/contract (random-ball) (-> ball?)
  (make-ball/random-velocity (random-x) (random-y)))

;; click : MouseEvent -> Boolean
;; returns true if mevt is "button-down"
(define (click? mevt)
  (string=? mevt "button-down"))

;; handle-mouse : WorldState XCoord YCoord MouseEvent -> WorldState
;; Inserts a new ball on mouse click, at given mouse x and y
;; (new Ball center may be adjusted so it's always fully in-scene)
(define/contract (handle-mouse balls x y mevt)
  (-> (listof ball?) XCoord? YCoord? mouse-event? (listof ball?))
  (cond
    [(click? mevt) (add-ball/random-vel x y balls)]
    [else balls]))

(define/contract (add-ball/random-vel x y balls)
  (-> XCoord? YCoord? (listof ball?) (listof ball?))
  (cons (make-ball/random-velocity (adjx x) (adjx y)) balls))

(define (main)
  (big-bang (list (random-ball))
    [on-mouse handle-mouse]
    [on-tick next-World]
    [to-draw World->Image]))

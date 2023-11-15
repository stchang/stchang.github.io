#lang racket

(provide (rename-out [parse parse450js]
                     [run run450js]))

(require rackunit)

;; A 450jsExpr is one of
;; - Number
;; - String
;; - (list '+ 450jsExpr 450jsExpr)
;; - (list '- 450jsExpr 450jsExpr)

(define (is-add? sym) (equal? sym '+))
(define (is-sub? sym) (equal? sym '-))

;; A 450jsAST is one of:
;; - (num Number)
;; - (str String)
;; - (add 450jsAST 450jsAST)
;; - (sub 450jsAST 450jsAST)
(struct num [val] #:transparent)
(struct str [val] #:transparent)
(struct add [lft rgt] #:transparent)
(struct sub [lft rgt] #:transparent)

;; parse : 450jsExpr -> 450jsAST
(define (parse s)
  (match s
    [(? number?) (num s)]
    [(? string?) (str s)]
    [`(+ ,x ,y) (add (parse x) (parse y))]
    [`(- ,x ,y) (sub (parse x) (parse y))]))

(check-equal? (parse 1) (num 1))
(check-equal? (parse '(+ 1 2)) (add (num 1) (num 2)))
(check-equal? (parse '(+ (- 1 2) (- 3 4)))
              (add (sub (num 1) (num 2))
                    (sub (num 3) (num 4))))

;; A 450jsResult is one of
;; - Number
;; - String
;; - NaN
;; Interp: possible results of a CS450js program
;; Note: NaN is a valid result, but not a valid program (for now)
(struct nan [])
(define NaN (nan))

;; 450jsResult -> String
(define (res->str x)
  (cond
    [(string? x) x]
    [else (~a x)]))

;; 450jsResult -> Number or NaN
(define (res->num x)
  (cond
    [(number? x) x]
    [else NaN]))

;; 450+ : 450jsResult 450jsResult -> 450jsResult
(define (450+ x y)
  (cond
    [(or (string? x) (string? y)) (string-append (res->str x) (res->str y))]
    [else
     (define xnum (res->num x))
     (define ynum (res->num y))
     (if (or (nan? xnum) (nan? ynum)) nan (+ xnum ynum))]))

;; 450- : 450jsResult 450jsResult -> 450jsResult
(define (450- x y)
  (define xnum (res->num x))
  (define ynum (res->num y))
  (if (or (nan? xnum) (nan? ynum))
      NaN
      (- xnum ynum)))

;; run : 450jsAST -> 450jsResult
;; Computes the result of running the given CS450js program AST
(define (run p)
  (match p
    [(num n) n]
    [(str s) s]
    [(add x y) (450+ (run x) (run y))]
    [(sub x y) (450- (run x) (run y))]))

(define eval450js (compose run parse))

(check-equal? (eval450js 1) 1)
(check-equal? (eval450js '(+ 1 2)) 3)
(check-equal? (eval450js '(+ (- 1 2) (- 3 4))) -2)

;; with js-style coercions: string, number

;; "adding" strings
(check-equal? (eval450js '(+ "hello" " world")) "hello world")

;; "adding" strings and numbers
(check-equal? (eval450js '(+ 100 "grand")) "100grand")
(check-equal? (eval450js '(+ "cs" 450)) "cs450")

;; "sub" with any string is NaN
(check-equal? (eval450js '(- "hello" "world")) NaN)
(check-equal? (eval450js '(- "hello" 100)) NaN)
(check-equal? (eval450js '(- 100 "world")) NaN)

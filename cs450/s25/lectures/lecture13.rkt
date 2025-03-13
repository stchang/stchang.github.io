#lang racket

(require 2htdp/image
         rackunit)

;; CS450 Spring 2025
;; Thurs March 13
;; Lecture 13: Function Arithmetic and the Lambda Calculus

((compose sqrt add1) 8)

(apply
 above
 (build-list
  5
  (compose (curryr rectangle 50 "solid" "blue")
           (curry * 20)
           add1)))

;; --------------------------------------------------
;; church numerals

;; A ChurchNum is a function with two arguments:
;; f : a function to apply
;; base : a base ("zero") value to apply to
;;
;; A "Church" representation of specific number applies the given
;; function that number of times

(define czero
  (lambda (f base) base))

(define cone
  (lambda (f base) (f base)))

(define ctwo
  (lambda (f base) (f (f base))))

(define cthree
  (lambda (f base) (f (f (f base)))))

;; cplus : ChurchNum ChurchNum -> ChurchNum
(define cplus
  (lambda (m n)
    (lambda (f base)
      (m f (n f base)))))

(define (church-num->num cnum)
  (cnum add1 0))
(define cnum->num church-num->num)

(check-equal? (cnum->num (cplus cone ctwo)) 3)

;; cplus1 : ChurchNum -> ChurchNum
(define cplus1
  (lambda (n)
    (lambda (f base)
      (f (n f base)))))

(check-equal? (cnum->num (cplus1 czero)) 1)
(check-equal? (cnum->num (cplus1 cone)) 2)
(check-equal? (cnum->num (cplus1 cthree)) 4)

;; The "minus1" function is more difficult to understand.
;; A Church numeral applies a function n times.
;; So "minus1" must return a function that applies its parameter n - 1 times.
;; This is achieved by building a container around f and x, which is initialized
;; in a way that omits the application of the function the first time.
(define cminus1
  (lambda (n)
    (lambda (f base)
      ((n (lambda (g) (lambda (h) (h (g f)))) (lambda (u) base)) (lambda (u) u)))))

(check-equal? (cnum->num (cminus1 cone)) 0)
(check-equal? (cnum->num (cminus1 ctwo)) 1)
(check-equal? (cnum->num (cminus1 czero)) 0)

(define cminus
  (lambda (m n)
    (n cminus1 m)))

(check-equal? (cnum->num (cminus cone czero)) 1)
(check-equal? (cnum->num (cminus cthree cone)) 2)

(define cmult
  (lambda (m n)
    (lambda (f base)
      (m (curry n f) base))))

(check-equal? (cnum->num (cmult cone ctwo)) 2)
(check-equal? (cnum->num (cmult ctwo cthree)) 6)
(check-equal? (cnum->num (cmult cthree (cmult ctwo cthree))) 18)

#;(define exp
  (lambda (m n)
    (lambda (f base)
      (n (lambda (f2) (m f2 base)) base))))

;(check-equal? (->num (exp two three)) 8)

;(define zero
;  (lambda (f) (lambda (base) base)))
;
;(define one
;  (lambda (f) (lambda (base) (f base))))
;
;(define two
;  (lambda (f) (lambda (base) (f (f base)))))
;
;(define three
;  (lambda (f) (lambda (base) (f (f (f base))))))
;
;(define plus
;  (lambda (m) (lambda (n) (lambda (f) (lambda (base) ((m f) ((n f) base)))))))
;
;(define plus1
;  (lambda (n) (lambda (f) (lambda (base) (f ((n f) base))))))
;
;(define mult
;  (lambda (m) (lambda (n) (lambda (f) (lambda (base) ((m (n f)) base))))))

;; A ChurchBool is a function of two arguments where
;; "true" returns the first argument, and
;; "false returns the second argument

(define ctrue (lambda (a b) a))
(define cfalse (lambda (a b) b))

(define (cbool->bool cb)
  (cb true false))

;; "and" for church bools
;; if p is true, then we want (and p q) = q
;; and indeed (p q p) = q (bc it returns the first arg)
;; if p is false, then we want (and p q) = p
;; and indeed (p q p) = p (bc it returns the second arg)
(define cand
  (lambda (p q) (p q p)))

(check-equal? (cbool->bool (cand ctrue ctrue)) true)
(check-equal? (cbool->bool (cand cfalse ctrue)) false)
(check-equal? (cbool->bool (cand ctrue cfalse)) false)
(check-equal? (cbool->bool (cand cfalse cfalse)) false)

;; "or" for church bools
;; if p is true, then we want (or p q) = p
;; and here (p p q) = p (bc it returns the first arg)
;; if p is false, then we want (or p q) = q
;; and indeed (p p q) = q (bc it returns the second arg)
(define cor
  (lambda (p q) (p p q)))
(check-equal? (cbool->bool (cor ctrue ctrue)) true)
(check-equal? (cbool->bool (cor cfalse ctrue)) true)
(check-equal? (cbool->bool (cor ctrue cfalse)) true)
(check-equal? (cbool->bool (cor cfalse cfalse)) false)

;; church "if" is just same as "true" or "false"
;; if p = true, result is first branch
;; if p = false, result is second branch
(define cif
  (lambda (p a b) (p a b)))

;; A ChurchCons is a 2-arg function that produces a 1-arg function
;; the two arguments are the data values
;; the 1-arg function is a function that is applied to (i.e., "selects")
;; the data values

(define cpair
  (lambda (x y)
    (lambda (get)
      (get x y))))

(define cfirst
  (lambda (clst)
    (clst (lambda (x y) x))))

(define csecond
  (lambda (clst)
    (clst (lambda (x y) y))))

(check-equal? (cnum->num (cfirst (cpair cone ctwo))) 1)
(check-equal? (cnum->num (csecond (cpair cone ctwo))) 2)

;; program that keeps producing "itself", i.e., an infinite loop
#;((λ (x) (x x))
   (λ (x) (x x)))

;; program that prints itself (with helper function)
(define (print2x str)
  (printf "(~a\n ~v)\n" str str))

((λ (x) (print2x x))
 "(λ (x) (print2x x))")

;; program that prints itself (inlined)
((λ (x) (printf "(~a\n ~v)\n" x x))
 "(λ (x) (printf \"(~a\\n ~v)\\n\" x x))")

#lang racket

;; CS420: Monday Nov 16, 2020
;; Lecture 18: Turing Machines and Recursion

;; We've said many times in this class that the Turing machine model of
;; computation models programs, so a TM can do whatever a
;; Python/Java/Racket/C++ program can do. But programmers in those languages
;; use recursion, and it's not clear how this is possible with our formal
;; states-and-transitions model of TMs. Today we'll show how TMs can indeed
;; implement recursive programs (without needing to explicitly allow
;; self-references in the definition of TMs).

;; We will demonstrate this by implementing a function equivalent to the
;; following recursive factorial function without using recursion (i.e.,
;; without self-reference):
(define (factorial/recursion n)
  (if (zero? n)
      1
      (* n (factorial/recursion (sub1 n)))))

;; We will do this incrementally. 

;; ----------------------------------------------------------------------------
;; 1) To start, we create a program that can print itself, analogous to this
;; English sentence:

;; Print out two copies of the following, the second on in quotes:   <-- fun
;; “Print out two copies of the following, the second on in quotes:” <-- arg

;; Here is a roughly equivalent program that prints "itself"
((λ (x) (printf "(~a\n ~v)\n" x x))
 "(λ (x) (printf \"(~a\\n ~v)\\n\" x x))")

;; This program does not use recursion (i.e., has no self-references), yet
;; somehow knows enough about "itself" to print "itself".

;; ----------------------------------------------------------------------------
;; 2) But printing is not very interesting. Instead, we change to program to
;; repeatedly call "itself" (still without using recursion):

;; piece of code that repeatedly calls itself (without recursion) (will loop)
#;((λ (x) (x x))
   (λ (x) (x x)))

;; The code above does the same thing as this recursive function (it loops),
;; except it does not use recursion:
;(define (f x) (f x))
;(f 0)

;; ----------------------------------------------------------------------------
;; 3) Instead of looping, we can also pass "itself" to some other function

;; this `mk-recursive-fn` function passes "itself" to some `f` (which can then
;; call "itself" with an argument `v`, or not)
(define mk-recursive-fn
  (λ (f)
    ((λ (x) (f (λ (v) ((x x) v))))
     (λ (x) (f (λ (v) ((x x) v)))))))

;; ----------------------------------------------------------------------------
;; 4) What can we do with `mk-recursive-fn`? Well the Recursion Theorem
;; formally states that if we want to create a recursive function "R", we
;; instead create a "T" that is the same as "R" except it has an extra argument
;; that is "itself".

;; Concretely, if we want this:
#;(define (factorial n) ;; an "R" TM, according to Recursion Thm
    (if (zero? n)
        1
        (* n (factorial (sub1 n)))))

;; We instead write this:
(define ((factorial/itself ITSELF) n) ; a "T" TM, according to Recursion Thm
  (if (zero? n)
      1
      (* n (ITSELF (sub1 n)))))

;; ----------------------------------------------------------------------------
;; 5) Then, we derive "R" from "T", by passing it to `mk-recursive-fn`!
(define factorial/no-recursion
  (mk-recursive-fn factorial/itself))

;; Let's check that the recursive and non-recursive versions are equivalent:
(require rackunit)
(check-equal? (factorial/no-recursion 0) (factorial/recursion 0))
(check-equal? (factorial/no-recursion 1) (factorial/recursion 1))
(check-equal? (factorial/no-recursion 2) (factorial/recursion 2))
(check-equal? (factorial/no-recursion 3) (factorial/recursion 3))
(check-equal? (factorial/no-recursion 4) (factorial/recursion 4))
(check-equal? (factorial/no-recursion 5) (factorial/recursion 5))

;; What's happening here? Well another way to state the Recursion Theorem is
;; that every TM that has a TM as input and output has a "fixed
;; point". Similarly, every function that consumes and outputs a function has a
;; fixed point.

;; It turns out that `mk-recursive-fn` has another name, the "Y Combinator",
;; which is a "fixed point finder".  Specifically, the `factorial/recursion` is
;; just the fixed point of `factorial/no-recursion` and the Y Combinator
;; converts the latter to the former, just like the Recursion Theorem says is
;; possible.

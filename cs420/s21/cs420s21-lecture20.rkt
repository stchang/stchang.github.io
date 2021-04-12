#lang racket

;; ----------------------------------------------------------------------------
;; CS420 Spring 2021: Monday April 12, 2021
;; Lecture 20: Turing Machines and Recursion
;; ----------------------------------------------------------------------------

;; This semester, we've repeatedly said that:
;;
;;    "Turing machines model programs"
;;
;; i.e., a TM can do whatever a Python/Java/Racket/C++ program can do.

;; But programmers in those languages use recursion!

;; Can the TMs we've been studying model recursive programs?

;; This is what we'll investigate today!

;; SPOILER ALERT: 
;; The TMs we've been studying can already model recursive programs!

;; Even though there's no explicit mention of self-referencing in the formal
;; definitions we've seen.

;; NOTE: the following code requires Racket to run

;; ----------------------------------------------------------------------------
;; 1) A Self-printing Program

;; To start, we create a program that can print itself.

;; Such a program is analogous to this English sentence:

;; Print out two copies of the following, the second one in quotes:   <-- fun
;; “Print out two copies of the following, the second one in quotes:” <-- arg

;; Here, the first line represents a "function",
;; where "the following" represents a "function parameter",
;; and the second line is the "argument" to the function.

;; Here is an actual program that prints "itself":

(define (print2x-2ndunquoted str) (printf "(~a\n ~v)\n" str str))

((λ (the-following) (print2x-2ndunquoted the-following))
 "(λ (the-following) (print2x-2ndunquoted the-following))")

;; Note that the "print2x" helper function is only to help readability.
;; I could have inlined it and the program would still print itself:
((λ (the-following) (printf "(~a\n ~v)\n" the-following the-following))
 "(λ (the-following) (printf \"(~a\\n ~v)\\n\" the-following the-following))")

;; This is evidence that a program can know about "itself", even without recursion.

;; ----------------------------------------------------------------------------
;; 2) Other Self-Aware Programs

;; Printing is not super interesting, from a practical perspective.

;; In the same spirit, however, here are some other programs, that know
;; about "itself", without using recursion.
 
;; Program that goes into infinite loop by repeatedly calling itself (no recursion)
#;((λ (x) (x x))
   (λ (x) (x x)))

;; For reference, here is a self-looping program with recursion.
;; (define (f x) (f x))
;; (f f)

;; self looping program, but calls an extra "f" function before each loop
;; This is called the "Y combinator"
(define Y
  (λ (f)
    ((λ (x) (f (λ (v) ((x x) v))))
     (λ (x) (f (λ (v) ((x x) v)))))))

;; ----------------------------------------------------------------------------
;; 3) The Recursion Theorem

;; How do the non-recursive, yet self-aware programs above relate to the recursive
;; programs that programmers more traditionally write?

;; For example, here is a standard recursive factorial function:
(define (factorial n) ; "R" in the Recursion Theorem
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

;; According to the Recursion Theorem (Thm 6.3 in the textbook), we can write
;; the same function without recursion by:

;; a) writing a non-recursive function with an extra "itself" argument
(define ((factorial/itself ITSELF) n) ;; "T" in the Recursion Theorem
  (if (zero? n)
      1
      (* n (ITSELF (- n 1)))))

;; b) converting this non-recursive version of the function to one that is
;; equivalent to the original recursive function that we wanted.

;; c) More specifically, this "conversion" finds the "fixed point" of
;; factorial/itself, which is guaranteed to exist (Theorem 6.8 from the textbook)

;; d) It turns out that the Y combinator, from above, does exactly this. It
;; finds the fixed point of functions whose argument is another function.

;; Here, fact2 is the fixed point of factorial/itself. It is found by passing
;; factorial/itself to the Y combinator (the "fixed point finder").

;; Thus fact2 is equivalent to the recursive "factorial" definition above.
;; Try a few tests to confirm this!
(define (fact2 n) ((Y factorial/itself) n))
(fact2 0)
(fact2 1)
(fact2 2)
(fact2 3)
(fact2 4)
(fact2 5)

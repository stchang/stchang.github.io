#lang racket

;; CS450 Spring 2025, lecture 14
;; March 25, Recursion in the Lambda Calculus
(require rackunit)

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


;; regular factorial function using Racket's "built-in" recursion
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n)))))

;; "factorial-maker" function, where "the recursive" call is a parameter;
;; returns "the factorial function"
(define factorial-maker
  (lambda (THE-RECURSIVE-CALL)
    (lambda (n)
      (if (zero? n)
          1
          (* n (THE-RECURSIVE-CALL (sub1 n)))))))

(define mk-recursive-fn ; "Y Combinator" makes recursive fns
  (lambda (f)
    ((λ (x) (f (lambda (v) ((x x) v))))
     (λ (x) (f (lambda (v) ((x x) v)))))))

;; factorial, without using Racket's recursion
(define factorial/no-recursion
  (mk-recursive-fn factorial-maker))

;; check that no-recursion factorial fn is same as regular factorial
(for ([n 10])
  (check-equal? (factorial n)
                (factorial/no-recursion n)))

;; no-recursion factorial function, with everything inlined (no defs)
(for ([n 10])
  (check-equal? (factorial n)
                ; below is same as (factorial/no-recursion n)
                (((lambda (f) ; Y Combinator
                    ((λ (x) (f (lambda (v) ((x x) v))))
                     (λ (x) (f (lambda (v) ((x x) v))))))
                  (lambda (THE-RECURSIVE-CALL) ; factorial "maker" fn
                    (lambda (n)
                      (if (zero? n)
                          1
                          (* n (THE-RECURSIVE-CALL (sub1 n)))))))
                 n)))

;; map --------------------------------------------------
(define map-maker
  (lambda (THE-RECURSIVE-CALL)
    (lambda (f lst)
      (cond
        [(empty? lst) empty]
        [else (cons (f (first lst))
                    (THE-RECURSIVE-CALL f (rest lst)))]))))

(define mk-multiarg-recursive-fn ; "Y Combinator" for multi-arg recursive fn
  (lambda (f)
    ((λ (x) (f (lambda args-lst (apply (x x) args-lst))))
     (λ (x) (f (lambda args-lst (apply (x x) args-lst)))))))

;; map, without using Racket's recursion
(define map/no-recursion
  (mk-multiarg-recursive-fn map-maker))

(check-equal? (map add1 (list 1 2 3))
              (map/no-recursion add1 (list 1 2 3)))
#lang scribble/lp

Programmers regularly employ both abstract data types and pattern matching. Unfortunately, the two are often at odds with each other. This short post shows how @tech{match expanders} in Racket provide an easy solution to the problem.

@section{A Functional Queue}

Consider the following functional queue @cite{pfds}, implemented with an @racket[internal-queue] structure containing two lists, a front list and a rear list.
    
@chunk[<queue>
       (struct internal-queue (front rear))
       
       (define empty-queue (internal-queue null null))

       (define (queue-empty? q) (null? (internal-queue-front q)))
       
       (define (checkf f r) 
         (if (null? f) 
             (internal-queue (reverse r) null)
             (internal-queue f r)))
       
       (define/match (enqueue q x) 
         [((internal-queue f r) _) (checkf f (cons x r))])
       
       (define/match (head q) 
         [((internal-queue f _))
          (if (null? f) (error 'head "empty queue") (car f))])
       
       (define/match (tail q)
         [((internal-queue f r))
          (if (null? f) (error 'tail "empty queue") (checkf (cdr f) r))])]

The @racket[enqueue] function adds an element to the rear of the queue while @racket[head] and @racket[tail] get and remove, respectively, an element from the front. When the front list is empty, as checked by @racket[checkf], then the rear list is reversed and set as the new front list and the rear list is reset to empty.

@chunk[<queue-tests>
       (let ([q (enqueue empty-queue 10)])
         (check-equal? (head q) 10)
         (check-true (queue-empty? (tail q))))]
    
@section{A Problem}

The presented queue implementation uses pattern matching on the internal structure of the queue. However, say the queue implementer wants to represent the queue with an abstract data type and @racket[provide]s only @racket[empty-queue], @racket[enqueue], @racket[head], and @racket[tail] as the interface to the queue. Now a client of the queue cannot use pattern matching anymore. The implementer could additionally provide the @racket[internal-queue] structure, but this would leak the internal representation of the queue and break the abstraction.

@section{A Solution}

Traditionally, language implementers have resolved this tension between abstract data types and pattern matching with views @cite["wadler" "okasaki"]. In Racket, @tech{match expanders} achieve the same goal.

Say I want to match the following pattern:
    
@chunk[<queue-match>
       (match (enqueue empty-queue 10) [(queue hd tl) ...])]
  
where @racket[hd] and @racket[tl] are respectively the head and tail of the queue returned by @racket[head] and @racket[tail].

We can implement this with a match expander that uses the @racket[app] @racket[match] pattern:

@chunk[<queue-match-expander>
       (define-match-expander queue
           (syntax-rules ()
             [(_ hd tl) (app (λ (q) (list (head q) (tail q))) (list hd tl))]))]
    
Now if the author of the queue library provides the @racket[queue] match expander, a user of the queue can use pattern matching without having to know the internal representation of the queue.

@chunk[<queue-match-expander-tests>
       (match (enqueue empty-queue 100)
         [(queue h t)
          (check-equal? h 100)
          (check-true (queue-empty? t))])]

To take it one step further, we can use Racket's macros to define a new @racket[define-view] form, which generalizes the match expander above to enable the definition of arbitrary views with any number of accessors.

@chunk[<define-view-v1>
       (define-syntax (define-view-v1 stx)
         (syntax-case stx ()
           [(_ view-name accessor ...)
            (with-syntax ([(x ...) (generate-temporaries #'(accessor ...))])
              #'(define-match-expander view-name
                  (syntax-rules ()
                    [(_ x ...) 
                     (app (λ (y) (list (accessor y) ...)) (list x ...))])))]))
       
       (define-view-v1 queue-v1 head tail)
       
       (match (enqueue empty-queue 200)
         [(queue-v1 h t)
          (check-equal? h 200)
          (check-true (queue-empty? t))])]
    
@section{A Small Optimization}

Sometimes, to reduce extraneous data destructuring, programmers write accessors that return multiple pieces of data at once. For example:

@chunk[<head+tail>
       (define/match (head+tail q)
         [((internal-queue f r))
          (if (null? f)
              (error 'head+tail "empty queue")
              (values (car f) (checkf (cdr f) r)))])
       
       (let-values ([(h t) (head+tail (enqueue empty-queue 300))])
         (check-equal? h 300)
         (check-true (queue-empty? t)))]

We can support these kinds of accessors in @racket[define-view] by assuming that if we're only given a single accessor, it may return multiple values. We do this by adding a new clause that defines a match expander which matches any number of subpatterns. We use @racket[(... ...)] to indicate that we want to use literal ellipses, rather than ellipses from a pattern in the @racket[syntax-case].

@chunk[<define-view-v2>
       (define-syntax (define-view-v2 stx)
         (syntax-case stx ()
           [(_ view-name values-returning-accessor)
            #'(define-match-expander view-name
                (syntax-rules ()
                  [(_ field (... ...)) 
                   (app (λ (y) (call-with-values 
                                (λ () (values-returning-accessor y))
                                list)) 
                        (list field (... ...)))]))]
           [(_ view-name accessor ...)
            (with-syntax ([(x ...) (generate-temporaries #'(accessor ...))])
              #'(define-match-expander view-name
                  (syntax-rules ()
                    [(_ x ...) 
                     (app (λ (y) (list (accessor y) ...)) (list x ...))])))]))
       
       (define-view-v2 queue-v2 head tail)
       
       (match (enqueue empty-queue 400)
         [(queue-v2 h t)
          (check-equal? h 400)
          (check-true (queue-empty? t))])
       
       (define-view-v2 queue-v3 head+tail)
       
       (match (enqueue empty-queue 500)
         [(queue-v3 h t)
          (check-equal? h 500)
          (check-true (queue-empty? t))])]
       

@section{Conclusion}

Here is how to put all the code together.

@chunk[<*>
       (require racket rackunit)
       <queue>
       <head+tail>
       <queue-tests>
       <queue-match-expander>
       <queue-match-expander-tests>
       <define-view-v1>
       <define-view-v2>]
  
@(bibliography
  (bib-entry #:key "wadler"
             #:author "Philip Wadler"
             #:title "Views: A way for pattern matching to cohabit with data abstraction"
             #:location "14th POPL"
             #:date "1987") 
  (bib-entry #:key "okasaki"
             #:author "Chris Okasaki"
             #:title "Views for Standard ML"
             #:location "ML Workshop"
             #:date "1998")
  (bib-entry #:key "pfds"
             #:author "Chris Okasaki"
             #:title "Purely Functional Data Structures"
             #:is-book? #t
             #:date "1986"
             #:location "Cambridge University Press"))

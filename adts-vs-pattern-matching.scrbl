#lang scribble/lp

Programmers regularly employ both abstract data types and pattern matching. Unfortunately, the two are often at odds with each other. This short post shows how @tech{match expanders} in Racket provide an easy solution to the problem.

@section{A functional queue}

Consider this functional queue @cite{pfds}, implemented with an @racket[internal-queue] structure containing two lists, a front list and a rear list.
    
@chunk[<queue>
       (struct internal-queue (front rear))
       
       (define empty-queue (internal-queue null null))

       (define (queue-empty? q) (null? (internal-queue-front q)))
       
       (define (checkf f r) 
         (if (null? f) 
             (internal-queue (reverse r) null)
             (internal-queue f r)))
       
       (define (enqueue q x) 
         (match q 
           [(internal-queue f r) (checkf f (cons x r))]))
       (define (head q) 
         (match q 
           [(internal-queue f _) 
            (if (null? f) 
                (error 'head "empty queue")
                (car f))]))
       (define (tail q)
         (match q 
           [(internal-queue f r) 
            (if (null? f) 
                (error 'tail "empty queue") 
                (checkf (cdr f) r))]))]

The @racket[enqueue] function adds an element to the rear of the queue while @racket[head] and @racket[tail] get and remove, respectively, an element from the front. When the front list is empty, as checked by @racket[checkf], then the rear list is reversed and set as the new front list and the rear list is reset to empty.
    
@section{The Problem}

Now say the queue implementer wants to represent the queue as an abstract data type and @racket[provide]s only @racket[empty-queue], @racket[enqueue], @racket[head], and @racket[tail] as the interface to the queue. The problem is that a user of the queue cannot employ pattern matching anymore. The implementer could additionally provide the @racket[internal-queue] structure, but this would leak the internal representation of the queue.

@section{A Solution}

Traditionally, language implementers have resolved this tension between abstract data types and pattern matching with views @cite["wadler" "okasaki"]. In Racket, @tech{match expanders} elegantly to achieve the same goal.

Say I want to match the following pattern:
    
@chunk[<queue-match>
       (match (enqueue empty-queue 10) [(queue hd tl) ...])]
  
where @racket[hd] and @racket[tl] are respectively the head and tail of the queue returned by @racket[head] and @racket[tail].

We can implement this with a match expander and the @racket[app] @racket[match] pattern:

@chunk[<queue-match-expander>
       (define-match-expander queue
           (syntax-rules ()
             [(_ hd tl) (app (λ (q) (list (head q) (tail q))) (list hd tl))]))
       (match (enqueue empty-queue 10)
         [(queue h t)
          (check-equal? h 10)
          (check-true (queue-empty? t))])]
    
To take it one step further, we can use Racket's macros to define a new @racket[define-view] form, which generalizes the match expander above and defines arbitrary views with any number of accessors.

@chunk[<define-view>
       (define-syntax (define-view stx)
         (syntax-case stx ()
           [(_ view-name accessor ...)
            (with-syntax ([(x ...) (generate-temporaries #'(accessor ...))])
              #'(define-match-expander view-name
                  (syntax-rules ()
                    [(_ x ...) (app (λ (y) (list (accessor y) ...)) (list x ...))])))]))
       (define-view queue2 head tail)
       (match (enqueue empty-queue 200)
         [(queue2 h t)
          (check-equal? h 200)
          (check-true (queue-empty? t))])]
    
@section{An optimization}

@section{Conclusion}

  @chunk[<*>
         (require racket rackunit)
         <queue>
         <queue-match-expander>
         <define-view>]
  
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
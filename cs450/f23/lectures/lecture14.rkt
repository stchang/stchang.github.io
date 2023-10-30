#lang racket

;; cs450 fall 2023, section 2, Oct 25, lecture 14 code
(require rackunit)

;; A BinaryTree<X> (BT) is one of
;; - NONE
;; - (tree BT<X> X BT<X>)
(struct empty-tree [])
(define NONE (empty-tree))

(struct tree [left val right] #:transparent)
(define (bt? x) (or (empty-tree? x) (tree? x)))

;; BT template
#;(define (bt-fn bt)
  (cond
    [(empty-tree? bt) ...]
    [(tree? bt) ... (bt-fn (tree-left bt)) ...
                ... (tree-val bt) ...
                ... (bt-fn (tree-right bt)) ...]))

(define (bt-height bt)
  (cond
    [(empty-tree? bt) 0]
    [(tree? bt) (add1 (max (bt-height (tree-left bt))
                           (bt-height (tree-right bt))))]))

(define TREE1 (tree NONE 1 NONE))
(define TREE2 (tree NONE 2 NONE))
(define TREE3 (tree NONE 3 NONE))
(define TREE4 (tree NONE 4 NONE))
(define TREE5 (tree NONE 5 NONE))
(define TREE6 (tree NONE 6 NONE))
(define TREE7 (tree NONE 7 NONE))
(define TREE123 (tree TREE1 2 TREE3))
(define TREE425 (tree TREE4 2 TREE5))
(define TREE637 (tree TREE6 3 TREE7))
(define TREE (tree TREE425 1 TREE637))

(check-equal? (bt-height NONE) 0)
(check-equal? (bt-height TREE1) 1)
(check-equal? (bt-height TREE123) 2)
(check-equal? (bt-height (tree TREE1 0 NONE)) 2)
(check-equal? (bt-height (tree NONE 0 TREE1)) 2)
(check-equal? (bt-height (tree TREE123 0 TREE1)) 3)
(check-equal? (bt-height TREE) 3)

;; bt->lst : BT<X> -> Listof<X>
;; converts given tree to a list of values inorder
(define (bt->lst bt)
  (cond
    [(empty-tree? bt) empty]
    [(tree? bt)
     (append (bt->lst (tree-left bt))
             (cons (tree-val bt)
                   (bt->lst (tree-right bt))))]))

(check-equal? (bt->lst TREE) (list 4 2 5 1 6 3 7))

;; bt->lst/pre : BT<X> -> Listof<X>
;; converts given tree to a list of values preorder
(define (bt->lst/pre bt)
  (cond
    [(empty-tree? bt) empty]
    [(tree? bt)
     (define left-lst (bt->lst/pre (tree-left bt)))
     (define right-lst (bt->lst/pre (tree-right bt)))
     (cons (tree-val bt) (append left-lst right-lst))]))

(check-equal? (bt->lst/pre TREE) (list 1 2 4 5 3 6 7))

;; bt->lst/post : BT<X> -> Listof<X>
;; converts given tree to a list of values postorder
(define (bt->lst/post bt)
  (cond
    [(empty-tree? bt) empty]
    [(tree? bt)
     (define left-lst (bt->lst/post (tree-left bt)))
     (define right-lst (bt->lst/post (tree-right bt)))
     (append left-lst right-lst (list (tree-val bt)))]))

(check-equal? (bt->lst/post TREE) (list 4 5 2 6 7 3 1))

;; bt-all? : p? BT<X> -> Bool
;; Returns true if the given pred returns true for all values of the given bt
(define (bt-all? p? bt)
  (cond
    [(empty-tree? bt) true]
    [else
     (and (p? (tree-val bt))
          (bt-all? p? (tree-left bt))
          (bt-all? p? (tree-right bt)))]))


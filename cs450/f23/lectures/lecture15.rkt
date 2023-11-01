#lang racket

;; cs450 fall 2023, section 2, Oct 30, lecture 15 code
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

;; btdo : BT<X> (X -> Void) -> Void
;; applies given fn to tree values inorder
(define (btdo f bt)
  (cond
    [(empty-tree? bt) (void)]
    [(tree? bt) (btdo f (tree-left bt))
                (f (tree-val bt))
                (btdo f (tree-right bt))]))

(btdo displayln TREE)

;; pre-btdo : BT<X> (X -> Void) -> Void
;; applies given fn to tree values in preorder
(define (pre-btdo f bt)
  (cond
    [(empty-tree? bt) (void)]
    [(tree? bt) (f (tree-val bt))
                (pre-btdo f (tree-left bt))
                (pre-btdo f (tree-right bt))]))

(pre-btdo displayln TREE)

;; post-btdo : BT<X> (X -> Void) -> Void
;; applies given fn to tree values in preorder
(define (post-btdo f bt)
  (cond
    [(empty-tree? bt) (void)]
    [(tree? bt) (post-btdo f (tree-left bt))
                (post-btdo f (tree-right bt))
                (f (tree-val bt))]))

(post-btdo displayln TREE)

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

;; bt-has? : BT<X> X -> Bool
;; Returns true if the given bt has the given val
(define (bt-has? bt x)
  (cond
    [(empty-tree? bt) false]
    [(tree? bt)
     (or (equal? x (tree-val bt))
         (bt-has? (tree-left bt) x)
         (bt-has? (tree-right bt) x))]))

(check-true (bt-has? TREE 1))
(check-true (bt-has? TREE 4))
(check-true (bt-has? TREE 7))
(check-false (bt-has? TREE 8))

;; A BinarySearchTree<X> (BST) is one of
;; - NONE
;; - (tree BT<X> X BT<X>)
;; Invariant 1: for all values x in left tree, x < root val
;; Invariant 2: for all values y in right tree, y >= root val

;; bst-has? : BST<X> X -> Bool
;; Returns true if the given bst has the given val
(define (bst-has? bst x)
  (cond
    [(empty-tree? bst) false]
    [else
     (or (equal? x (tree-val bst))
         (if (< x (tree-val bst))
             (bst-has? (tree-left bst) x)
             (bst-has? (tree-right bst) x)))]))

(define TREE567 (tree TREE5 6 TREE7))
(define BST (tree TREE123 4 TREE567))
(check-true (bst-has? BST 1))
(check-true (bst-has? BST 4))
(check-true (bst-has? BST 7))
(check-false (bst-has? BST 8))

;; bt-all? : p? BT<X> -> Bool
;; Returns true if the given pred returns true for all values of the given bt
(define (bt-all? p? bt)
  (cond
    [(empty-tree? bt) true]
    [else
     (and (p? (tree-val bt))
          (bt-all? p? (tree-left bt))
          (bt-all? p? (tree-right bt)))]))

(define (f p? x)
  (and (p? x)
       ((curry > 10) x)))
;; valid-bst? : BT<X> -> Bool
;; Returns true if the given bt is a bst
(define (valid-bst? bt)
  (define (valid-bst/p? p? bt)
    (cond
      [(empty-tree? bt) true]
      [else
       (and (p? (tree-val bt))
            (valid-bst/p?
             (conjoin p? (curry > (tree-val bt)))
             #;(lambda (x)
               (and (p? x)
                    (> (tree-val bt) x)))
             (tree-left bt))
            (valid-bst/p?
             (conjoin p? (curry <= (tree-val bt)))
             #;(lambda (x)
               (and (p? x)
                    (<= (tree-val bt) x)))
             (tree-right bt)))]))
  (valid-bst/p? (lambda (x) true) bt)

#;(cond
    [(empty-tree? bt) true]
    [else
     (and (bt-all? (curry > (tree-val bt)) (tree-left bt))
          (bt-all? (curry <= (tree-val bt)) (tree-right bt))
          (valid-bst? (tree-left bt))
          (valid-bst? (tree-right bt)))]))

(check-true (valid-bst? BST))
(check-true (valid-bst? TREE123))
(check-false (valid-bst? TREE))
(check-true (valid-bst? TREE1)) ; singleton

;; left tree values are less than node-data, and
;; right tree values are greater than node-data,
;; but not a bst
(define SUPERFICIAL-BST
  (tree
   (tree TREE3 2 TREE1)
   4
   TREE567))
(check-false (valid-bst? SUPERFICIAL-BST))

;; the left and right trees are BSTs,
;; but not a BST
(define ALMOST-BST
  (tree TREE123
        2
        TREE567))
(check-false (valid-bst? ALMOST-BST))

;; insert : BST<X> X -> BST<X>
(define (bst-insert bst x)
  (cond
    [(empty-tree? bst) (tree NONE x NONE)]
    [else
     (if (< x (tree-val bst))
         (tree (bst-insert (tree-left bst) x)
               (tree-val bst)
               (tree-right bst))
         (tree (tree-left bst)
               (tree-val bst)
               (bst-insert (tree-right bst) x)))]))

(check-true (valid-bst? (bst-insert BST 8)))
(check-true (bst-has? (bst-insert BST 8) 8))
(check-true (valid-bst? (bst-insert BST 0)))
(check-true (bst-has? (bst-insert BST 0) 0))
(check-true (valid-bst? (bst-insert BST 1))) ; dup
(check-true (bst-has? (bst-insert BST 1) 1))

;; Forests --------------------------------------------------
;; A Forest<X> is one of:
;; - empty
;; - (cons BT<X> Forest<X>)
;; i.e., it's a list of binary trees

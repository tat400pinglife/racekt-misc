; class 11/5 notes

#lang eopl
; this is a definition of a regular binary tree

(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))



; bnf definition

; <bintree> :== <leaf> \ <interior node>
; <leaf-node> :== <number>
; <interior-node> :== <symbol> <bintree> <bintree>

; pertaining to homework question: compute sum of all leaf nodes

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
         (+ (leaf-sum left) (leaf-sum right))))))


; test tree

(define tree-a
  (interior-node 'a (leaf-node 3) (leaf-node 3)))

(define tree-b
  (interior-node 'b (leaf-node -1) tree-a))

(define tree-c
  (interior-node 'c  tree-b (leaf-node 1)))

(define tree-d
  (interior-node 'd tree-c tree-c))



; exercise 2.4 (*) implement a bintree-to-list procedure for binary trees, so
; (bintree-to-list tree-a) returns the list (interior-node a (leaf-node 3) (leaf-node 4))

; so how would you tackle this question
; tree recursion with conditions from the bnf definition where a tree is either a leaf node or an interior node


(define bintree-to-list
  (lambda (tree)
      (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right) ;past three lines contain the references for the values in the bintree
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right)
                           )))))
                     


; 2.5 (**) use cases to write a max-interior, which takes a binary tree
; of numbers with at least one interior node and returns the symbol
; associated with an interior node with a maximal leaf sum

; suppose you first make a list of pairs where the first element of the pair
; is the symbol and the second be the leaf-sum of the symbol

; then mapping cadr on that list will give us a list of purely leaf-sums
; taking accumulate to get the max of that list will give us a sum
; that we can reverse loop-up on our original list


(define max-interior
  (lambda (tree)
     (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (list (list key (leaf-sum interior-node))
                           (max-interior left)
                           (max-interior right
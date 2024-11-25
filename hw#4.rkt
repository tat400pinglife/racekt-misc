; Hw#4

;1. write a program to input a logical expression built using (at most) the logical connectives and, or, not, and implies
;   and to output a logically equivalent expression using (at most) 'and' and not.

;   You need to start an induction definition and an associated datatype for logical expressions.

;2. Read section 1.3.1 in EOPL and solve exercise 1.19

; 1.19: Write a procedure free-vars that takes a list structure representing an expression in
;       the lambda calculus syntax given above and returns a set (a list without duplicates) of all the variables that
;       occur free in the expression. Similarly, write a procedure bound-vars that returns a set of all the
;       variables that occur bound in its argument.

;3. Read section 1.3.2 in EOPL and solve exercise 1.31 and 1.32

; 1.31: 


; ALL OF THIS IS IN SEPARATE FILE "lexical addresses WIP"
#lang eopl

; first lets define what our program will do:
; It will accpet an logical expression and output an altered version with only not and 'and' (no or)
; e.g: (not (or A B)) --> (and (not A) (not B))
;      (not (and A B)) --> (or (not A) (not B))

; lets define the bnf definition of our expression
; 
; <exp> :== (var)
;       :== (and var var)
;       :== (or var var) == (not (and (not a) (not b)))
;       :== (implies var var) == (not A or B) ≡ (not (and A (not B)))
;       :== (not var)
;
; this is our accepted language where the final state will only contain the not and 'and'
;
; (and (implies A B) (or C D))
; (and (not (and A (not B))) (or C D))
;
; So the idea for the code is to detect what kind of expression is inputted and transform the expression by demorgan
; and replace the expression to one that is accepted and then recusively exmaine the vars of the expression and change them if needed.

;; Define constructors, classifiers, selectors for logical expressions as lists

(define (make-and left right)
  (list 'and left right))

(define (make-or left right)
  (list 'or left right))

(define (make-not expr)
  (list 'not expr))

(define (make-implies left right)
  (list 'implies left right))

(define (make-var symbol)
  symbol)

;; Classifiers

(define (and? expr)
  (and (pair? expr) (eq? (car expr) 'and)))

(define (or? expr)
  (and (pair? expr) (eq? (car expr) 'or)))

(define (not? expr)
  (and (pair? expr) (eq? (car expr) 'not)))

(define (implies? expr)
  (and (pair? expr) (eq? (car expr) 'implies)))

(define (var? expr)
  (symbol? expr))

;; Selectors

(define (left expr)
  (cadr expr))

(define (right expr)
  (caddr expr))

(define (not-expr expr)
  (cadr expr))


; A quick rundown on how demorgans transforms expressions:
; (A or B) == (not (not A and not B))
; (A implies B) == (not A or B) == (not (A and not B))
; All other cases have logical operations that don't conflict with the end goal

(define (transform expr)
  (cond
    ((or? expr)
     (make-not (make-and (make-not (transform (left expr)))
                         (make-not (transform (right expr))))))
    ((implies? expr)
     (transform (make-not (make-and (transform (left expr))
                          (make-not (transform (right expr)))))))
    ((and? expr)
     (make-and (transform (left expr))
               (transform (right expr))))
    ((not? expr)
     (make-not (transform (not-expr expr))))

    ((var? expr) expr)))


(define test
  '(implies A (and (or B C) (not D))))



























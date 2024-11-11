;class #6 notes
#lang eopl
; Lambda Calculus
;
;note* this is in the eopl chapter 1.3 reading on lambda calculus
;
; <exp> ::= <identifier>, which we will take to be scheme identifiers
;       ::= (lambda (<identifier>) exp)
;       ::= (<exp> <exp>)
;
; Definition of free variables in a lambda calculus expression
; - if exp is an identifier x, then x is free in exp
; - if is (lambda (y) exp'), then x is free in exp if it is free in exp' AND if x != y
; - if exp is (exp_1, exp_2) then x is free in exp if it is free in exp_1 OR it is free in exp_2
;
; here is an example of x free in exp_1, bound in exp_2 and therefore free in (exp_1, exp_2) (...) neck hurt look at book. We'll represent sets as lists without repetition

; We wish to write a program which inputs a lambda calc exp e, and returns the set of all variables which are free in e
;
; It seems that some set "infrastruture".
; 1. We'll want (remove x s) to remove the bound variable from (free-vars body) when e is (lambda(x) body
; 2. We'll want (union-set e_1 e_2) to handle e = (e_1 e_2). In addition, the function (element-of? x s) will be needed to compute the union os two sets.
;
;
; Some "infrastructure" for lambda calculus expressions will also be helpful. Here we'll design a data strucure:
; - Constructors, selectors, and classifiers.
;
; Constructors: make-lambda, make-application
; Selectors: get-formal, get-body, operator, operand (if you wondering why theres no get, he got tired of writing it)
;
; (lambda     (y)        exp') like (lambda (x) (+ x 1))
;            formal      body
;
; To implement the classifiers, as well as the selectors, we need to decide on a represntation.
;
(define make-lambda
  (lambda (formal body) ; Assume that body is a valid lambda calc. representation built using these constructors
    (list 'lambda (list formal) body)))

(define make-app
  (lambda (e1 e2) ; Assume e1 and e2 are valid lambda calculus representations built using these constructors
    (list e1 e2)))

(define first
  car)
(define second
  cadr)
(define third
  caddr)

(define get-formal
  (lambda (exp) ; exp is of the form (lambda(x) body)
    (first (second exp))))

(define get-body
  (lambda (exp) (third exp)))

(define operator
  first)
(define operand
  second)

(define lambda-exp?
  (lambda (exp)
  (eq? (first exp) 'lambda)))


; identifiers can be recognized using the primitive symbol '?'
; ----------
; and we can avoid the need for an application recognizer by making this the else clause of our synthax-directed program
; ---------------------
; What about some details for the set ops?
; (union-set s1 s2) can be organized as a cdr-down on s1, returning s2 when s1 is empty. For each element x of s1 we need to detect if x is an element of s2.
;  -> Do we want to use eq? or equal?
;
; well we use eq? since we're working with lists of variables - which are atoms - so eq? is the right choice
;
(define union-set
  (lambda (x y)
    (define helper
     (lambda (x y result)
      (cond ((null? x) result)
            ((not (member (car x) y)) (helper (cdr x) y result))
            (else (helper (cdr x) y (append result (list(car x)))))))) ; alternative to have the call wiht append first and take out the not
    (helper x y '())
    )
  )
; now we can spit out some code
; if x not in s2, cons x onto (union-set (cdr set1) set2)
; otherwise skip x
;
; Consider (remove x s)
;  -> eq? or equal?
;  -> do we need to continue searching s once the first occurence of x is found
;
; No, we're working with sets represented as lists without repeated elements
; -> return s if x not in s
;
(define remove
  (lambda (x s)
     (cond ((null? s) '()) ; checks if there are items in list
          ((eq? x (car s)) (remove x (cdr s))) 
          (else (cons(car s) (remove x (cdr s)))
          )
     )))
; pre: exp is a lambda calc representation built using our constructors
(define free-vars
  (lambda (exp)
    (cond ((symbol? exp) (list exp))
          ((lambda-exp? exp)
           (let ((x (get-formal exp)))
             (remove x (get-body exp))))
          (else (union-set ( operator exp) (operand exp)))
          )))
; post: free variables in exp

; Proof by structural induction an exp, using the BNF definition of lambda calc expression
(define test_lambda
  (make-lambda 'x '(x y z w)))

(define lmbda
  (make-lambda 'x (make-lambda 't '(x y z))))

(define test_e
  (make-app '(x y) '(x z)))
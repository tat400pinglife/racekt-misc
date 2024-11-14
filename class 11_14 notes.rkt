; classs 11/14 notes

; reference to 2.11 in eopl (parse-expression  problem)

; This is from the book:

; Given two lambda forms: e_1:(lambda (p) (+ p x)) and e_2:(lambda (q) (+ q x)), you can observe that these are logically eqv

; But if we just change the bound variable without the parameter we get a different expr

; Now consider the expression resulting from substituting (* p 3) for x:

; f_1:(lambda (p) (+ p (* p 3))) , f_2:(lambda (q) (+ q (* p 3)))

; are these expression logically the same/identical?

; No, the p in lambda(p) has been captured by the binder, but in the other expression 'p' isn't bound anywhere

; So how would we model logical exquivalence here?
; A MODEL for an exp. f: an interpretation of the symbols occuring in e

; Seeking a model which distinguishes f_1 from f_2...
; lets take + to denote addition and * to denote multiplication
; we'll also take the symbol 3 to denote the value three

; for these expression, the only other parts open to interpretation are the free variables

; Take e_1 and e_2 to make a model:
; + is addition, p is any number, and x is any number. (this is constrained by + which forces the type)
; This will be the same case for e_2 as they are logically equivalent with different vars. So we can use the same model

; but this doesn't hold for f_1 and f_2 where the same model cannot be used as p is defined locally while defined globally in the other.

; what would the solution then? How to define substitution so that meanings are preserved

; problem: p in (* p 3) is free, but it has been captured by the binder

; Suppose we change the formal q to r: (lambda (r) (+ r (* p 3))), so the solution is to avoid the bindings from the lambda

#lang eopl

(define-datatype primitive primitive?
  (add-prim)
  (mul-prim)
  )

(define-datatype expression expression?
  (lit-exp
   (datum number?))
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (primapp-exp
   (prim primitive?)
   (rand1 expression?)
   (rand2 expression?))
  (app-exp
   (rator expression?)
   (rand expression?))
  )

(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec ((subst (lambda (exp)
                      (cases expression exp
                        (var-exp (id)
                                 (if (eqv? id subst-id) subst-exp exp))
                        (lambda-exp (id body)
                                    (lambda-exp id (subst body)))
                        (app-exp (rator rand)
                                 (app-exp (subst rator) (subst rand)))
                        (lit-exp (datum)
                                 (lit-exp datum))
                        (primapp-exp (prim rand1 rand2)
                                     (primapp-exp prim (subst rand1) (subst rand2)))
                        ))))
      (subst exp)
      )))


; parse should input concrete to get abstract, vice-versa for unparse

; so how should our function parse an expression
; ex: (lambda(p) (+ p x))

; to make e_1 suitable for input to lambda-calculus-subst, we need to parse it.
; lets go over it one part at a time:

;          Concrete                Abstract (code to deliver abstract syntax)
;         ----------              ----------
;             +                   (add-prim)
;             p                   (var-exp 'p)
;             x                   (var-exp 'x)

; now we have the pieces, how to we make (+ p x)

;          (+ p x)                (primapp-exp (add-prim) (var-exp 'p) (var-exp 'x))
;    (lambda (p) (+ p x))         (lambda-exp 'p (primapp-exp (add-prim) (var-exp 'p) (var-exp 'x)))

; looking at our lambda-calculus-subst function we know that the abstract is our first parameter
; the second would be our expression to be subbed in
; the third would be the symbol being replaced by the expression


; lex add

#lang racket

; quick thing on lexical addresses

; var = (var : depth posiiton)

; where depth is the amount of lambda functions that contains it excluding the one that references it
; position refers to the order the variable appears in the depth of its environment

; each variable has been replaced with (v: d p) where d is the depth in the environment
; containing v and p is the position of v in that sublist

(define test
'(lambda
     (x y)
;---------------(1)
  (
   (lambda(a)
;--------------(2)
     (x (a y))
   ) ;(: 1 0) (: 0 0) (: 1 1)
;--------------(2)
   x)
  )); (: 0 0)
;--------------(1)

; for this specific example we have a 3 element list (lambda, (x y), and ((lambda (a) (x (a y))) x)

; Now how should this problem be approached?

; Lets start with defining our input:
; It will be a list of lists containing lambda expressions

; Then the process will start with a way to keep track of what variables are bounded in the lambda expressions



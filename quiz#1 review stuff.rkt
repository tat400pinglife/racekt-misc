; Quiz one review
;
; Question: Given the input of two lists without repitition (sets)
;           output the intersect of the two lists in order of the first inputted list
;
;
; What should the design process look like?
;
; idk just make it concise and do it before the code pos prof
;
; What should the induction look like?
; Firstly it shouldn't be a step by step of the process
; [MUST use the IH]
; 
; We may assume that the call (intersect-set (cdr x) y) correctly computes the intersection of (cdr x)
; and y. So we get x intersection y by consing (car x) if (car x) in y, and otherwise
; (intersect-set (cdr x) y) is the correct intersection.
;
; What's an informal (ie, no full-fledged guess invariant (GI) needed) development of an iterative program?
;
; Design Idea: 1. your variable's names and design roles (ie. what do the variables represent?)
;              2. A termination idea
;
; Termination can be to shrink the not yet processed list until it is empty
;
;
; 
;
;
;
#lang eopl 
(define L1
  '(1 2 3 4 5 6 7 8 9 10))

(define L2
  '(2 4 6 8 10))


(define intersect
  (lambda (x y)
    (define helper
     (lambda (x y result)
      (cond ((null? x) result)
            ((not (member (car x) y)) (helper (cdr x) y result))
            (else (helper (cdr x) y (append result (list(car x)))))))) ; alternative to have the call wiht append first and take out the not
    (helper x y '())
    )
  )
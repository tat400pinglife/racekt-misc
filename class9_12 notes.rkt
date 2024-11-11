;class#4 note 9/12
(define x
  '(
    (1(2 3(4 5 6 7)) 8)
    (9 10 (11 12 ))
    13)
  )
(car(cdr(cdr(car(cdr(car x))))))

; Structural induction
;*Frist, we need a way of identifying proper components of a structure. For a tree t, for now, these are (car t) and (cdr t)
;*Once we can identify these, the induction arguement proceeds nicely:
; IH: -> Assume the result for the proper components
; IS: -> Use these hypotheses to establish the overall result.


;Take this example: Show that and, or, and not can be expressed purely in and and not
;We need an inductive definition of the class of logic expressions before we can start
;l is the smallest class of expressions containing T,F, variable x,y,z which is closed {x,y,z}
;Under the operations and, or, not -> as follows:
;  if E_1 and E_2 belong to l, then (E_1 and E_2) belongs to l
;  if E_1 and E_2 belong to l, then (E_1 or E_2) belong to l
;  if E_1 belong to l, then (-E_1) belong to l
;  if E_1 and E_2 belong to l, then (E_1 implies E_2) belong to l
;This is the inductive step

;Now to prove by structural induction
;Basis step (for simplest expressions)
;    x has no occurrences of implies or (or), so of course its is logically equivalent to an expression that doe snot use implies or (or)
;    One can say this is a "vacuously true"
;    The same holds for y,z,T,F
;Note that expressions are logically equivalent if they have the same truth table
;Structural Induction hypothesis-> as expected for a structural induction
;Induction step-> 4 cases: (p or q), (p and q), (not p), (p implies q)
;1. (p or q) By the SIH , there is p' is logically equivalent to p, which doesnt use implies or (or)
;    vice versa for q
;    by De Morgan's law (p' or q') = not(not p' and not q'), which has no or and implies

;2. (p implies q) by the SIH, *same thing for case 1*
;    We can remember that (A implies B) is logically equivalen to (not A or B)
;    This result can be put through De Morgan's law
;The other cases are already proven with no (or) or implies in the expression


; 10/29 class notes

; LEXICAL ADDRESSES

(define add1
  (lambda (x)
    (+ x 1)))

(
 (lambda (x)
  ((lambda(y)
    (
     (lambda (x)
       (x y))
     x)
    ) 17)
   )
 add1)

; This will take function add1 for x into free variable in lambda calculus
; so we get ((lambda (y)
;            ((lambda (x)
;               (x y))
;              add1)
;            ) 17 )

; this will then evaulate the 17 into y where it will go to the free var inside (x y)
; then taking x for add1 to get (add1 17)


(lambda (x)
  (lambda (y)
    ((lambda (x)
       ((x:00)
        (y:10)))
       (x:10))))

; each variabel has been replaced with (v: d p) where d is the depth in the environment
; containing v and p is the position of v in that sublist


(lambda (x y)
;---------------(1)
  ((lambda(a)
;--------------(2)
     (x (a y))) ;(: 1 0) (: 0 0) (: 1 1)
;--------------(2)
   x)); (: 0 0)
;--------------(1)

; so our first scope will be x will 

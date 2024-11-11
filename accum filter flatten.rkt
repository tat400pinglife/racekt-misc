; things to know for midterm

; higher order functions like map, filter, accumulate, and flatten

(define accumulate
  (lambda (op init seq)
    (cond((null? seq) init)
         (else
          (op (car seq)
              (accumulate op init (cdr seq))))
         )))

; accumulate will take an inital starting point and operation and apply it to every element
; so (accumulate + 0 lst) will add all elements to zero and sum up
; same thing but for mult (accumulate * 1 lst)
(define filter
  (lambda (pred l)
    (cond ((null? l)l)
          ((pred (car l))
           (cons (car l)
                 (filter pred (cdr l))))
          (else (filter pred (cdr l)))
          )))

(define lst
  '(1 2 3 4 5 6 7 8 9 10))

(define flatten
  (lambda (mult-lst)
    (accumulate append '() mult-lst)))

(define llst
  '((1 2) (3 4) (5 6) (7 8) (9 10)))

(define lllst
  '( ((1 2)(3 4)) ((5 6)(7 8)) ((9 10)) )
  )

; map applies an operation on every given element in a list
; like (map even? lst) will replace everything in list with #t or f
; or (map + lst lst) will add every element to itself
; (map - lst) will negate very element
; (map * lst lst lst...) will return the product
; map can also be given a new function like
; (map (lambda (x) (modulo x 3) lst))

(define mod3
  (lambda (x)
    (modulo x 3)))






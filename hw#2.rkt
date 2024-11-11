;Hw#2

;After defining procedures dec
;pre: x is an integer
(define dec
  (lambda (x)
   (- x 1)))
;post: returns x-1

;and inc
;pre: x is an integer
(define inc
  (lambda (x)
    (+ x 1)))
;post: returns x+1

;1. write a recursive procedure "myADD" of two integers (x, y) which returns x+y using inc and dec
;pre: x and y are integers
(define myADD
  (lambda (x y)
    (if (= y 0)
        x
    (if (> y 0)
        ( myADD (inc x)(dec y))
        ( myADD (dec x)(inc y))))))
;post: returns the sum of x+y


;2. Write a recursive procedure sum-digits which inputs an integer x and which returns the sum if digits in x.
;pre: x is an integer
(define sum-digits
  (lambda (x)
    (if (= 0 x)
        0
        (+ (remainder (abs x) 10)(sum-digits (quotient (abs x) 10))))))
;post: returns the sum of all digits in x


;iterative version
;pre: x in an integer
(define sum-digits-iter
  (lambda (x)
    (define helper
  (lambda (x total)
          (cond
            ((= 0 x) total)
            ((not(= 0 x)) (helper (quotient (abs x) 10)(+ total (remainder (abs x) 10))))
              )))
    (helper x 0)))
;post: returns sum of digits in x
;3. Write a procedure "myLength" that returns the length of a list, without using the built-in length function
;pre: x is a list
(define myLength_built
  (lambda (x)
    (length x)))
; or
(define myLength
  (lambda (x)
    (if (null? x)
        0
        (+ 1 (myLength (cdr x))))))
;post: returns the length or numebr of elements in list x

;iter version
;pre: x ix a list
(define myLength-iter
  (lambda (lst l)
    (cond ((null? lst) l)
          (not (null? lst) (myLength-iter (cdr lst) (+ l 1))))))
;post: returns length of list

;4. write a recursive procedure that takes a list and integer and outputs the first n elements on the list
;pre: l is a list and n is an integer that is less than or equal to the length if l
(define first_n
  (lambda (l n)
    (if (= n 0)
        '()
    (cons (car l) (first_n(cdr l) (- n 1))))))

;5. Write a recursive procedure "myAppend" which inputs two lists x and y and returns a single list concatenating x and y
;   without using the built-in function append. How much time(O) does your pricedure take?
;pre: x and y are lists
(define list1
  '(1 2 3))
(define list2
  '(4 5 6))
(define myAppend
  (lambda (x y)
    (if (null? x)
         y
         (cons(car x)(myAppend (cdr x) y))
     )))






         
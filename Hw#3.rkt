;hw#3

;accum. filter
(define accumulate
  (lambda (op init seq)
    (cond((null? seq) init)
         (else
          (op (car seq)
              (accumulate op init (cdr seq))))
         )))

(define filter
  (lambda (pred l)
    (cond ((null? l)l)
          ((pred (car l))
           (cons (car l)
                 (filter pred (cdr l))))
          (else (filter pred (cdr l)))
          )))
;1. Write a program that reverses the order of a number without using lists
;pre: x is an integer
(define length_of
  (lambda (x)
        (if (= 0 x)
        0
        (+ 1 (length_of (quotient (abs x) 10))))))
(define opposite
  (lambda (x l)
        (if (= 0 x)
        0
        (+ (* (remainder (abs x) 10) (expt 10 (- l 1))) (opposite (quotient (abs x) 10) (- l 1))))))
;pot; output is x in reverse order

(define x
  123456789)

;iter version
(define opposite-iter
  (lambda (x)
    (define helper
      (lambda (x l total)
        (cond ((= 0 x) total)
              ((not (= 0 x)) (helper (quotient (abs x) 10) (- l 1) (+ total (* (remainder(abs x) 10) (expt 10 (- l 1))))))
              )))
    (helper x (length_of x) 0)))
;2. Write a procedure that outputs the reverse of a given list
;pre: x is a list
(define L1
  '(1 2 3 4 5 6 7 8 9))
(define (reverse_list lst)
  (define (reverse_iter lst output)
    (if (null? lst)
        output
        (reverse_iter (cdr lst)
                      (cons (car lst) output))))
  (reverse_iter lst '()))
;post: output is list reversed

;3. write a procedure that computes the height of a tree correspondinghg to a list of lists
; pre: t is a tree (list of lists)
(define tree
  '((1 2)(3 (4 (5 6)))))

(define height
  (lambda (x)
    (if (not (pair? x))
         0
         (+ 1 (max(height (car x)) 
                   (height (cadr x)))))))
; post: height of tree

;4. write a procedure that inputs a tree t, and returns the deep reverse of the tree
; Pre: tree is a list
(define deep_reverse
  (lambda (tree)
  (map
   (lambda (x)
         (if (list? x)
             (deep_reverse x)
             x))
       (reverse tree))))
; post: reverse of tree

;5. write a procedure to count the number of occurences of the atom a in a tree t
; pre: tree is a list, x is an atom
(define occurrences
  (lambda (tree x)
    (cond
    ((null? tree) 0) 
    ((not (pair? tree)) 
     (if (eq? tree x) 1 0))
    (else
     (+ (occurrences (car tree) x)
        (occurrences (cdr tree) x))))))
; post: number of occurrences of x in tree
     
;6. *tough* write a procedure that replaces the nth occurence of an atom a in a tree t by another atom b

; invoke occurences to stop at the
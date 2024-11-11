; practice questions (do recursive and iterate)
; Given a list output the list without any duplicates
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

(define L1
  '(1 a 2 3 a s d b 4 2 1))
(define tree
  '((1 2)(3 (4 (5 6)))))
; list is a list of atoms
(define remove-dup
  (lambda (lst)
    (cond ((null? lst) '()) ; checks if there are items in list
          ((member (car lst) (cdr lst)) (remove-dup (cdr lst))) ; checks if the car of the list is present in any other position in list
          (else (cons (car lst) (remove-dup (cdr lst))))))); if not in list cons the car into the result and cdr down list

(define remove-dup-iter
  (lambda (lst)
    (define helper
      (lambda (lst no_dup)
        (cond ((null? lst) no_dup)
              ((member (car lst) (cdr lst)) (helper (cdr lst) no_dup))
              (else (helper (cdr lst) (append no_dup (list(car lst))) )))))
    (helper lst '())))



; find max value in list
; pre; list is of integers
(define L2
  '(1 2 3 4 5 6 7))
(define max_list
  (lambda (lst)
    (cond ((null? lst) 0)
          (else (max (car lst)
                      (max_list (cdr lst)) )
                     )
          )))

(define max_list-iter
  (lambda (lst)
    (define helper
      (lambda (lst max)
        (cond ((null? lst) max)
              ((> (car lst) max) (helper (cdr lst) (car lst)))
              (else (helper (cdr lst) max)))))
    (helper lst 0)))
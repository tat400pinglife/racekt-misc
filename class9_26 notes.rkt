; class 9/26 notes

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


(define filter_acc
  (lambda (pred l)
    (accumulate (lambda (x y)
                  (if (pred x)
                      (cons x y)
                      y))
          cons (f x) y))
                '()
                l)
    ))

; theres a common platform here- being abstracted by accumulate
; where accumulate replaces the cdr loop
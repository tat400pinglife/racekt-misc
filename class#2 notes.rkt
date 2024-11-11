(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact(- n 1))))))

(cons 1 2 )
(car (cons 1 2))

(define x (cons (cons 1 2)(cons 3 4)))

(car (cdr x))

(define y (cons 1(cons 2 ( cons 3 'Cookie))))

; move me out of onedrive
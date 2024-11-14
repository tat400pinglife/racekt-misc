#lang eopl

; class 11/12 notes

(define empty-env
  (lambda ()
    (lambda (sym) (eopl:error 'apply-env "No binding for ~s" sym))))

(define extend-env
  (lambda (syms vals env)
    (lambda (sym)
      (let ((pos (list-find-position sym syms)))
        (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym))))))

;; Apply environment to retrieve the value associated with a symbol
(define apply-env
  (lambda (env sym)
    (env sym)))

;; Helper function to find the position of an item in a list
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; Helper function to find index using predicate
(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
             (if (number? list-index-r)
                 (+ list-index-r 1)
                 #f))))))

(define my-env
  (extend-env '(a b c) '(1 2 3) empty-env))
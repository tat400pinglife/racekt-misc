#lang eopl

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

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp (prim primitive?) (rands (list-of expression?)))
  )

(define-datatype primitive primitive?
  (add-prim)
  (subtract-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim))

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body) (eval-expression body (init-env))))
    ))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands) (let ((args (eval-rands rands env))) (apply-primitive prim args))))
      ))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env) (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      )))

(define init-env
  (lambda ()
    (extend-env '(i v x) '(1 5 10) (empty-env))))
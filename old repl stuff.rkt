;; Remove redundant manual datatype declarations
;; SLLGEN automatically generates these from the grammar
;(define-datatype program program?
;  (a-program (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp (datum number?))
;  (var-exp (id symbol?))
;  (primapp-exp (prim primitive?)
;               (rands (list-of expression?))))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (subtract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;; Remove manual AST conversion functions
;(define expression-to-list
;  (lambda (exp)
;    (cases expression exp
;      (lit-exp (datum) (list 'lit-exp datum))
;      (var-exp (id) (list 'var-exp id))
;      (primapp-exp (prim rands)
;        (list 'primapp-exp
;              (cases primitive prim
;                (add-prim () (list 'add-prim))
;                (subtract-prim () (list 'subtract-prim))
;                (mult-prim () (list 'mult-prim))
;                (incr-prim () (list 'incr-prim))
;                (decr-prim () (list 'decr-prim)))
;              (map expression-to-list rands))))))

;(define program-to-list
;  (lambda (prgm)
;    (cases program prgm
;      (a-program (exp)
;        (list 'a-program (expression-to-list exp))))))

;; Replace manual scan&parse with SLLGEN equivalent

#lang eopl

(define scheme-value?
  (lambda (v) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

(define list-find-position (lambda (sym los)
                             (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index (lambda (pred ls)
                     (cond ((null? ls) #f)
                           ((pred (car ls)) 0)
                           (else (let ((list-index-r (list-index pred (cdr ls))))
                                   (if (number? list-index-r)
                                       (+ list-index-r 1)
                                       #f))))))

(define empty-env
  (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))
(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))

(define scanner-spec-3-1
  '((white-sp
     (whitespace)                                    skip)
    (comment
     ("%" (arbno (not #\newline)))                   skip)
    (identifier
     (letter (arbno (or letter digit "?")))          symbol)
    (number
     (digit (arbno digit))                           number)))


(define grammar-3-1
  '((program
     (expression) a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",")  ")" )
     primapp-exp)
    (primitive ("+")
               add-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("add1")
              incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("minus")
               minus-prim)
    (primitive ("car")
               car-prim)
    (primitive ("cons")
               cons-prim)
    (primitive ("list")
               list-prim)
    (primitive ("cdr")
               cdr-prim)
    ))
;; Automatically generate datatype definitions from grammar
(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1)

;; Keep essential evaluator and environment logic
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (minus-prim () (if (null? (cadr args))
                      (* (car args) -1) (eopl:error "ERROR")))
      (list-prim () args)
      (car-prim () (if (null? (cadr args))
                    (car (car args)) (eopl:error "ERROR")))
      (cons-prim () (cons (car args) (cadr args)))
      (cdr-prim () (cdr (car args)))
      )))

;; Retain REPL integration with streamlined SLLGEN parsing
(define read-eval-print
  (sllgen:make-rep-loop "--> " eval-program
     (sllgen:make-stream-parser
      scanner-spec-3-1
      grammar-3-1)))

(define repl
  (lambda ()
    (read-eval-print)))

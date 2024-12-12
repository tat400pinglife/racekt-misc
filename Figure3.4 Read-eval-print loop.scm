#lang eopl

;; Section 2.3.3

;; abstract syntax trees for environments

(define scheme-value?
  (lambda (v) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

;; corresponding implementation of the environment data type

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




;; with all of this in place, we are asked to implement environment-to-list, as illustrated
;; in the text (in the current context, this is perhaps useful as a means of
;; exploring the behavior of the interpreter

(define environment-to-list
  (lambda (env)
    (cases environment env
      (empty-env-record () (list 'empty-env-record))
      (extended-env-record (syms vals env)
                           (list 'extended-env-record syms vals (environment-to-list env))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Section 3.1  -- this definition is now supplied by sllgen: see below
;
;;; program datatype
;
;(define-datatype program program?
;  (a-program (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp (datum number?))
;  (var-exp (id symbol?))
;  (primapp-exp (prim primitive?)
;               (rands (list-of expression?))))
;  
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (subtract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))
;
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Figure 3.3 -- specifications for scanner and grammar

(define scanner
  '((white-sp
     (whitespace)                                    skip)
    (comment
     ("%" (arbno (not #\newline)))                   skip)
    (identifier
     (letter (arbno (or letter digit "?")))          symbol)
    (number
     (digit (arbno digit))                           number)))


(define grammar
  '((program
     (expression) a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)
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
    (primitive ("equal?")
               equal?-prim)
    (primitive ("zero?")
               zero?-prim)
    ))




;; here is the interpreter from Figure 3.2

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

(define true-value? (lambda (x) (not (zero? x))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (identifier expr body)
               (let ((args (eval-rands expr env)))
                 (eval-expression body (extend-env identifier args env))))
      )))

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
      (equal?-prim () (if (equal? (car args) (cadr args)) 1 0))
      (zero?-prim () (if (zero? (car args)) 1 0))
      )))

; more 'better' way is to create a function "check-primitive" that will check for the correct number of args
; this will then have to be changed with the read-eval-print that will have its own dedicated error message
; that will display when a function check return #f

(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; Figure 3.4


;; (define scan&parse
;;   (sllgen:make-string-parser
;;    scanner
;;    grammar))

(sllgen:make-define-datatypes scanner grammar)

;; (define run
;;   (lambda (string)
;;     (eval-program
;;      (scan&parse string))))

(define read-eval-print
  (sllgen:make-rep-loop "--> " eval-program
     (sllgen:make-stream-parser
      scanner
      grammar)))

(define repl
  (lambda ()
    (read-eval-print)))


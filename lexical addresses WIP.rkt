; lex add

#lang racket

; quick thing on lexical addresses

; var = (var : depth posiiton)

; where depth is the amount of lambda functions that contains it excluding the one that references it
; position refers to the order the variable appears in the depth of its environment

; each variable has been replaced with (v: d p) where d is the depth in the environment
; containing v and p is the position of v in that sublist

(define test
'((lambda
     (x y)
;---------------(1)
  (
   (lambda(a)
;--------------(2)
     (x (a y w))
   ) ;(: 1 0) (: 0 0) (: 1 1)
;--------------(2)
   x)) ; (: 0 0)
  x)) 
;--------------(1)

; for this specific example we have a 3 element list (lambda, (x y), and ((lambda (a) (x (a y))) x)

; Now how should this problem be approached?

; Lets start with defining our input:
; It will be a list of lists containing lambda expressions

; Then the process will start with a way to keep track of what variables are bounded in the lambda expressions


; lets create a bnf definition that we have already used in the past

; <expr> =:: (<lambda> <var*> <expr>)
;        =:: (if <expr> <expr> <expr>)
;        =:: <var>
;        =:: <expr*>

(define lexical-address
  '(lambda (a b c) (if (eqv? b c) ((lambda (c) (cons a c)) a) b)))

(define answ
  '(lambda (a b c) (if (eqv? (b : 0 1) (c : 0 2)) ((lambda (c) (cons (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1)))
)


; lets first start with functions that will determine if a variable occurs free

;A variable x occurs free in E if and only if there is some use of x in E that is not bound by any
;declaration of x in E.

;A variable x occurs bound in an expression E if and only if there is some use of x in E that is
;bound by a declaration of x in E.

(define make-lamb
  (lambda (formal body)
    (list 'lambda formal body)
    )
  )

(define make-if
  (lambda (formal true false)
    (list 'if formal true false)
    )
  )

(define make-lexical
  (lambda (var depth pos)
    (if (eq? pos 'free)
        (list var pos)
        (list var depth pos)
        )
    )
  )

(define prefix
  (lambda (expr)
    (car expr)
    )
  )

(define param
  (lambda (expr)
    (cadr expr)
    )
  )

(define body
  (lambda (expr)
    (caddr expr)
    )
  )

(define condition
  (lambda (expr)
    (cadr expr)
    )
  )

(define true
  (lambda (expr)
    (caddr expr)
    )
  )

(define false
  (lambda(expr)
    (cadddr expr)
    )
  )

(define unreserved-symbol?
  (lambda (expr)
    (and (not (eq? (first expr) 'lambda)) (not (eq? (first expr) 'if)) (symbol? expr))
    )
  )


(define test1
  '(lambda (a) (x a y w)))

;; Helper to determine if a symbol is a variable (not a keyword or operation)
(define (is-variable? sym)
  (and (symbol? sym)  ;; Must be a symbol
       (not (member sym '(lambda if + - * / < > =)))))  ;; Exclude reserved keywords/operations

;; Collects bound variables from an expression
(define (get-bound-vars expr)
  (let loop ((expr expr) (bound-vars '()))
    (cond
      ;; Case: atomic expression (variable)
      ((symbol? expr) bound-vars)
      
      ;; Case: lambda expression (<lambda> <var*> <expr>)
      ((and (pair? expr) (eq? (prefix expr) 'lambda))
       (let* ((params (filter is-variable? (param expr)))  ;; Only include valid variables
              (body (body expr))
              (new-bound-vars (append params bound-vars)))
         (loop body new-bound-vars)))  ;; Add params to bound-vars and process body
      
      ;; Case: if expression (if <expr> <expr> <expr>)
      ((and (pair? expr) (eq? (prefix expr) 'if))
       (let* ((test-bound (loop (condition expr) bound-vars))
              (then-bound (loop (true expr) test-bound))
              (else-bound (loop (false expr) then-bound)))
         else-bound))  ;; Combine all branches
      
      ;; Case: list of expressions (<expr*>)
      ((pair? expr)
       (let* ((first-bound (loop (prefix expr) bound-vars))
             (rest-bound (loop (cdr expr) first-bound)))
         rest-bound))  ;; Combine bound-vars from all parts
      
      ;; Default: no bound variables
      (else bound-vars))))

;; Collects free variables from an expression
(define (get-free-vars expr)
  (let loop ((expr expr) (bound-vars '()) (free-vars '()))
    (cond
      ;; Case: atomic expression (variable)
      ((symbol? expr)
       (if (and (is-variable? expr) (not (member expr bound-vars)))  ;; Ensure it's a valid free variable
           (cons expr free-vars)  ;; Add to free-vars if not bound
           free-vars))  ;; Skip if not free or not a valid variable
      
      ;; Case: lambda expression (<lambda> <var*> <expr>)
      ((and (pair? expr) (eq? (prefix expr) 'lambda))
       (let* ((params (filter is-variable? (cadr expr)))  ;; Only include valid variables
              (body (caddr expr))
              (new-bound-vars (append params bound-vars)))
         (loop body new-bound-vars free-vars)))  ;; Extend bound-vars
      
      ;; Case: if expression (if <expr> <expr> <expr>)
      ((and (pair? expr) (eq? (prefix expr) 'if))
       (let* ((test-free (loop (condition expr) bound-vars free-vars))
              (then-free (loop (true expr) bound-vars test-free))
              (else-free (loop (false expr) bound-vars then-free)))
         else-free))  ;; Combine free-vars from all branches
      
      ;; Case: list of expressions (<expr*>)
      ((pair? expr)
       (let* ((first-free (loop (car expr) bound-vars free-vars))
             (rest-free (loop (cdr expr) bound-vars first-free)))
         rest-free))  ;; Combine free-vars from all parts
      
      ;; Default: no free variables
      (else free-vars))))

;; Test utility
(define (analyze-vars-separated expr)
  (list 'bound (get-bound-vars expr) 'free (get-free-vars expr)))

;; Test cases
(define expr1 '(lambda (x y) (if x (+ y 1) z)))   ; z is free
(define expr2 '(lambda (x) ((lambda (y) (+ x y)) z))) ; z is free
(define expr3 '(lambda (x) (if (< x 0) (* x x) (+ x z)))) ; z is free
(define expr4 '(lambda (x) ((lambda (y) (if y x z)) 0))) ; z is free

;; Test results
(display (analyze-vars-separated expr1)) ; '(bound (x y) free (z))
(newline)
(display (analyze-vars-separated expr2)) ; '(bound (x y) free (z))
(newline)
(display (analyze-vars-separated expr3)) ; '(bound (x) free (z))
(newline)
(display (analyze-vars-separated expr4)) ; '(bound (x y) free (z))
(newline)

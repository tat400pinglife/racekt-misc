;; Define constructors and selectors for identifiers and addresses

#lang eopl

;; Constructor for an identifier with a lexical address
(define (make-identifier name depth position)
  (if (eq? position 'free)
      (list name depth)         ;; Free variable: (variable: depth)
      (list name depth position))) ;; Bound variable: (variable: depth position)

;; Selectors for identifier components
(define (identifier-name id) (car id))
(define (identifier-depth id) (cadr id))
(define (identifier-position id) (if (= (length id) 3) (caddr id) 'free))

;; Helper function to check if a variable is bound and return depth and position
(define (occurs-bound? var env)
  (let loop ((env env) (depth 0))
    (cond
      ((null? env) #f)                     ;; Variable not found, so it’s free
      (else
       (let ((pos (find-position var (car env))))
         (if pos
             (list depth pos)              ;; Variable found with (depth position)
             (loop (cdr env) (+ depth 1)))))))) ;; Continue searching in outer scopes

;; Find the position of a variable in a list of parameters (single scope level)
(define (find-position var vars)
  (let loop ((vars vars) (pos 0))
    (cond ((null? vars) #f)              ;; Variable not found
          ((eq? var (car vars)) pos)     ;; Variable found at position `pos`
          (else (loop (cdr vars) (+ pos 1)))))) ;; Continue searching

;; Helper function to check if a variable is free
(define (occurs-free? var env)
  (not (occurs-bound? var env)))

;; Main function to convert an expression into its lexical address form
(define (lexical-address expr env depth)
  (cond
    ;; Case 1: Identifier (variable)
    ((symbol? expr)
     (let ((bound-info (occurs-bound? expr env)))
       (if bound-info
           (make-identifier expr (car bound-info) (cadr bound-info))  ;; Bound variable with depth and position
           (make-identifier expr 'free 'free))))                      ;; Free variable

    ;; Case 2: Lambda expression
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (let ((params (cadr expr))
           (body (caddr expr)))
       ;; Extend environment with the new scope (parameters) and increment depth
       (list 'lambda
             params
             (lexical-address body (cons params env) (+ depth 1)))))

    ;; Case 3: Function application
    ((pair? expr)
     (map (lambda (e) (lexical-address e env depth)) expr))  ;; Process each part of the application

    ;; Default: Return the expression as-is
    (else expr)))

;; Testing the lexical address transformation
(define test-expr
  '(lambda (x y)
     ((lambda (x)
        (x (a y w)))  ;; Inner function body
      x)))            ;; Application of inner lambda to `x`

;; Transform and display the expression
(define transformed-expr (lexical-address test-expr '() 0))

(display transformed-expr) ; Show the transformed expression with lexical addresses
(newline)

(define test1
  '(lambda (a) (x a y w)))

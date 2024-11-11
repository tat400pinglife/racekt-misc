#lang eopl
;; Define constructors, classifiers, selectors for logical expressions as lists

;; Constructor for 'and'
(define (make-and left right)
  (list 'and left right))

;; Constructor for 'or'
(define (make-or left right)
  (list 'or left right))

;; Constructor for 'not'
(define (make-not expr)
  (list 'not expr))

;; Constructor for 'implies' (A implies B)
(define (make-implies left right)
  (list 'implies left right))

;; Constructor for atomic variables (e.g., A, B, C)
(define (make-var symbol)
  symbol)

;; Classifiers (Predicates)

;; Check if the expression is 'and'
(define (and? expr)
  (and (pair? expr) (eq? (car expr) 'and)))

;; Check if the expression is 'or'
(define (or? expr)
  (and (pair? expr) (eq? (car expr) 'or)))

;; Check if the expression is 'not'
(define (not? expr)
  (and (pair? expr) (eq? (car expr) 'not)))

;; Check if the expression is 'implies'
(define (implies? expr)
  (and (pair? expr) (eq? (car expr) 'implies)))

;; Check if the expression is a variable
(define (var? expr)
  (symbol? expr))

;; Selectors

;; Get the left operand of an 'and', 'or', or 'implies' expression
(define (left expr)
  (cadr expr))

;; Get the right operand of an 'and', 'or', or 'implies' expression
(define (right expr)
  (caddr expr))

;; Get the operand of a 'not' expression
(define (not-expr expr)
  (cadr expr))

;; Transformation function: Convert 'or' and 'implies' into 'and' and 'not'
(define (transform-to-and-not expr)
  (cond
    ;; If it's an 'or' expression, apply De Morgan's law
    ((or? expr)
     (make-not (make-and (make-not (transform-to-and-not (left expr)))
                         (make-not (transform-to-and-not (right expr))))))

    ;; If it's an 'implies' expression, transform (A implies B) to (not A or B)
    ((implies? expr)
     (transform-to-and-not (make-and (make-not (transform-to-and-not (left expr)))
                                    (transform-to-and-not (right expr)))))

    ;; If it's an 'and' expression, recursively transform its components
    ((and? expr)
     (make-and (transform-to-and-not (left expr))
               (transform-to-and-not (right expr))))

    ;; If it's a 'not' expression, recursively transform its component
    ((not? expr)
     (make-not (transform-to-and-not (not-expr expr))))

    ;; If it's a variable, return it as is
    ((var? expr) expr)))

;; Example usage: The input and output are lists
(define example-expr 
  (list 'and 
        (list 'implies 'A 'B) 
        (list 'or 'C 'D)))

;; Transform the example expression (list input, list output)
(define transformed-expr (transform-to-and-not example-expr))

;; Output the result as a list
(display transformed-expr) 
(newline)

;; Define constructors and selectors for identifiers and addresses

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
(define (find-variable var env depth)
  (let loop ((env env) (current-depth depth))
    (cond
      ((null? env) (list 'free 0)) ;; Free variable
      ((pair? (car env))
       (let ((position (find-position var (car env))))
         (if position
             (list current-depth position) ;; Found variable with (depth position)
             (loop (cdr env) (+ current-depth 1)))))
      (else (list 'free 0)))))

;; Find position of a variable in a list
(define (find-position var vars)
  (let loop ((vars vars) (pos 0))
    (cond ((null? vars) #f)             ;; Not found
          ((eq? var (car vars)) pos)    ;; Found at position `pos`
          (else (loop (cdr vars) (+ pos 1)))))) ;; Continue searching

;; Main function to convert an expression into its lexical address form
(define (lexical-address expr env depth)
  (cond
    ;; Case 1: Identifier (variable)
    ((symbol? expr)
     (let ((address (find-variable expr env 0)))
       (if (eq? (car address) 'free)
           (make-identifier expr 'free 'free)    ;; Free variable: (var: free)
           (make-identifier expr (car address) (cadr address))))) ;; Bound variable: (var: depth position)

    ;; Case 2: Lambda expression
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (let ((params (cadr expr))
           (body (caddr expr)))
       ;; Update the environment with the current lambda's parameters
       (list 'lambda
             params
             (lexical-address body (cons params env) depth))))

    ;; Case 3: Function application
    ((pair? expr)
     (map (lambda (e) (lexical-address e env depth)) expr))

    ;; Default: Return the expression as-is
    (else expr)))

;; Testing the lexical address transformation
(define test-expr
  '(lambda (x y)
     ((lambda (a)
        (x (a y w)))  ;; `w` is free, `x` and `y` bound in outer scope, `a` bound in inner scope
      x)))

;; Transform and display the expression
(define transformed-expr (lexical-address test-expr '() 0))

(display transformed-expr) ; Show the transformed expression with lexical addresses
(newline)

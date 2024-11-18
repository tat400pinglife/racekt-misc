;; Define an empty environment that returns #f if a symbol is not found
(define empty-env
  (lambda ()
    (lambda (sym) #f))) ;; Return #f for unbound variables

;; Extend environment with a list of symbols and corresponding values
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

;; Memoized Fibonacci function using environment model
(define memo-fib
  (lambda (n)
    ;; Use letrec to define fib-helper as a recursive function
    (letrec ((fib-helper
              (lambda (n env)
                (cond
                 ;; Base cases
                 ((= n 0) 0)
                 ((= n 1) 1)
                 ;; Check if the result is already cached in the environment
                 (else (let ((cached (apply-env env n)))
                         (if cached
                             cached  ;; Return cached value if found
                             ;; Compute if not cached and store result in new extended environment
                             (begin
                               ;; Display computation message
                               (display "Computing Fibonacci term ")
                               (display n)
                               (newline)
                               ;; Compute fib(n-1) and update the environment
                               (let* ((fib-n1 (fib-helper (- n 1) env))
                                      (env1 (extend-env (list (- n 1)) (list fib-n1) env))
                                      ;; Now compute fib(n-2) with the updated environment
                                      (fib-n2 (fib-helper (- n 2) env1))
                                      (result (+ fib-n1 fib-n2)))
                                 ;; Return the computed result after updating the environment
                                 (apply-env (extend-env (list n) (list result) env1) n))))))))))
      ;; Start with an empty environment
      (fib-helper n (empty-env)))))
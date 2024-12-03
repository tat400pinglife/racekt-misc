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

;; Section 3.1
(define maximal-prefix
  (lambda (s)
    (let ((max-index (- (string-length s) 1)))
      (letrec ((aux (lambda (j)
                      (cond
                        ((> j max-index) '())
                        (else (let ((next (string-ref s j)))
                                (cond ((or (eqv? next #\,)
                                           (eqv? next #\( )
                                           (eqv? next #\) )) '() )
                                      (else (cons next (aux (+ j 1)))))))))))
        (aux 0)))))




(define replace-each-top-level-comma
  (lambda (e)
    (let ((len (string-length e)))
      (letrec ((aux (lambda (curr-start curr depth)
                      (cond ((= curr len) (substring e curr-start curr))
                            ((eqv? (string-ref e curr) #\() (aux curr-start (+ curr 1) (+ depth 1)))
                            ((eqv? (string-ref e curr) #\)) (aux curr-start (+ curr 1) (- depth 1)))
                            ((not (eqv? (string-ref e curr) #\,)) (aux curr-start (+ curr 1) depth))
                            (else
                             (if (> depth 1) (aux curr-start (+ curr 1) depth)
                                 (string-append (string-append (substring e curr-start curr) " ")
                                                (aux (+ curr 1) (+ curr 2) depth))))))))
               (aux 0 1 1)))))
                            

(define extract-subexpressions
  (lambda (s)
    (let ((len (string-length s))
          (add1 (lambda (x) (+ 1 x)))
          (add2 (lambda (x) (+ 2 x)))
          (sub1 (lambda (x) (- x 1))))
      (letrec ((substrings-delimited-by-spaces
                (lambda (start curr)
                  (cond ((= curr len) (list (substring s start (sub1 curr))))
                        ((eqv? (string-ref s curr) #\space)
                         (cons (substring s start curr)
                               (substrings-delimited-by-spaces (add1 curr) (add2 curr))))
                        (else (substrings-delimited-by-spaces start (add1 curr)))))))
        (substrings-delimited-by-spaces 1 2)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primop-select
  (lambda (s)
    (cond ((equal? s "+") (add-prim))
          ((equal? s "-") (subtract-prim))
          ((equal? s "*") (mult-prim))
          ((equal? s "add1") (incr-prim))
          ((equal? s "sub1") (decr-prim)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define parse-program
  (lambda (exp)
    (a-program (parse-expression exp))))


(define parse-expression
  (lambda (exp)
    (let* ((len-exp (string-length exp))
           (pre (list->string (maximal-prefix exp)))
           (len-pre (string-length pre))
           (num (string->number pre)))
      (cond ((= len-pre len-exp)
             (if num (lit-exp num) (var-exp (string->symbol pre))))
            (else (primapp-exp (primop-select pre)
                               (map parse-expression (extract-subexpressions
                                                      (replace-each-top-level-comma
                                                       (substring exp len-pre len-exp))))))))))

(define grammar-3-1 '((program (expression) a-program) (expression (number) lit-
exp) (expression (id) var-exp) (expression (primitive "(" (separated-
list expression ",") ")" ) primapp-exp) (primitive ("+") add-
prim) (primitive ("-") subtract-prim) (primitive ("*") mult-
prim) (primitive ("add1") incr-prim) (primitive ("sub1") decr-prim)))

(define scan&parse (sllgen:make-string-parser scanner-spec-3-1 grammar-3-1))
(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1)
(define run (lambda (string) (eval-program (scan&parse string))))
(scan&parse "add1(2)")(a-program (primapp-exp (incr-prim) ((lit-exp 2))))> (run "add1(2)")
(define read-eval-print (sllgen:make-rep-loop "--> " eval-program (sllgen:make-stream-parser scanner-spec-3-1 grammar-3-1)))



(define run (lambda (x) (eval-program (parse-program x))))

(run '5)
(run '(add1 2))
(define read-eval-print
  (lambda () (begin (display "-- ") (write (eval-program (parse-program (read)))) (newline) (read-eval-print))))

(define init-env
  (lambda ()
    (extend-env '(i v x) '(1 5 10) (empty-env))))
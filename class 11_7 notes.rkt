;class 11/7 notes

; 2.2.2 in EOPL

#lang eopl

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lambda-exp ; We see that lambda-exp takes a concrete and an abstract input
   (id symbol?) ; like (lambda-exp 'x (var-exp 'x))
   (body expression?))
  (app-exp
   (rator expression?)
   (rand expression?)))

(define occurs-free?
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eq? id var))
      (lambda-exp (id body)
                  (and (not (eq? id var))
                       (occurs-free? var body)))
      (app-exp (rator rand)
               (or (occurs-free? var rator)
                   (occurs-free? var rand))))))



(define new_occurs-free?
  (lambda (var exp)
    (cases expression exp
      (var-exp (fred) (eq? fred var)) ; here we replace 'id' with fred, and it still compiles
      (lambda-exp (id body)
                  (and (not (eq? id var))
                       (occurs-free? var body))) ; in the case of 'cases' not all structures need to be dealt with, an else can end the cases prematurely
      (else #f)
     ; (app-exp (rator rand)
     ;          (or (occurs-free? var rator)
     ;              (occurs-free? var rand)))

      )))

(define parse-expression
  (lambda (datum)
    (cond ((symbol? datum) (var-exp datum))
          ((pair? datum)
           (if (eq? (car datum) 'lambda)
               (lambda-exp (caddr datum)
                           (parse-expression (caddr datum)))
               (app-exp
                (parse-expression (car datum))
                (parse-expression (cadr datum))))))))


(define unparse-expression
  (lambda (exp)
    (cases expression exp (var-exp (id) id)
      (lambda-exp (id body)
                  (list 'lambda (list id)
                        (unparse-expression body)))
      (app-exp (rator rand) (list (unparse-expression rator)
                                  (unparse-expression rand))))))

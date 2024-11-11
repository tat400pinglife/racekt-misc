#lang eopl


; exer. 2.7 (a) Define the data type and parse and unparse procedures for the above grammar.
(define-datatype expression expression?
  (lit-exp
   (num number?))
  (var-exp
   (id symbol?))
  (if-exp
   (test-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (lambda-exp
   (ids (list-of symbol?))
   (body expression?))
  (app-exp
   (rator expression?)
   (rands (list-of expression?))))

; relatively easy exercise: write the parse and unparse for this datatype (use the templates in class 11/7 notes)
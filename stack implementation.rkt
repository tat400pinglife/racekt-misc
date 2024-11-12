#lang eopl

; implementation of stack

(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
  (top symbol?)
  (rest stack?)))

(define push
  (lambda (new s)
    (non-empty-stack new s)))

(define pop
  (lambda (s)
    (cases stack s
      (empty-stack () (eopl:error 'pop "pop cannot be applied to the empty stack"))
      (non-empty-stack (top rest) rest))))

(define top
  (lambda (s)
    (cases stack s
      (empty-stack () (eopl:error 'top "top cannot be applied to the empty stack"))
      (non-empty-stack (top rest) top))))


(define s1
  (push 'a (empty-stack)))

(define s2
  (push 'b s1))

(define s3
  (push 'c s2))

(define s1_1
  (pop s1))


(define (module msg)
  (define (f1 x)) ; where a fucntion should be
  (define (f2 x))
  (define (f3 x))
  (define (dispatch m)
    (cond ((eq? m 1) f1)
          ((eq? m 2) f2)
          ((eq? m 3) f3)))
  (dispatch msg))
  
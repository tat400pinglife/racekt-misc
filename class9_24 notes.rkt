;class 9/24 notes


;we will construct a data structure to work with Aexp

;The data structure will typically contain constructors, selectors, and classifiers that
;serve as an interface between the application layer and the representation layer

;          Application layer               ("high level") -> routines to compute with 
;-------------------------------------                       rational numbers add-rat, mul-rat and so on
; Constructors Selectors classifiers    (Datatype layer)
;-------------------------------------                     rational numbers represented as
;           representation layer           (low level) ->  pairs (n, d) with n,d still 
;                                                          possibly having common factors
;
; This is just an exmaple using rational numbers as the target
;
;constructors
(define make-@
  (lambda (e1 e2)
    (list e1 '@ e2)))
;where e1 and e2 are representatives of elements of aexp (algebraic expressions)
(define make-$
  (lambda (e1 e2)
    (list e1 '$ e2)))

(define make-!
  (lambda (e1 e2)
    (list e1 '! e2)))

; selectors, definitions depend on constructor defs
(define first-operand
  (lambda(e)
    (car e)))

(define second-operand
  (lambda (e)
    (caddr e)))

(define operator
  (lambda (e)
    (cadr e)))

;classifiers
(define @-exp?
  (lambda (e)
    (eq? (operator e) '@)))

(define $-exp?
  (lambda (e)
    (eq? (operator e) '$)))

(define !-exp?
  (lambda (e)
    (eq? (operator e) '!)))

;Using these definintions, the process of desigining an interpreter
;for expressions in Aexp is much simpler than it would be otherwise

;The interpreter - named value - is the only way meaning is given to e in aexp.
;What is the meaning of (2 ! (3 $ 4)), well nothing for now

;First version - assume @, $, and ! are globally defined |
;Eg:
;pre: aexp is in Aexp
(define @ +)
(define $ *)
(define ! expt)

(define natnum?
  (lambda(x)
    (and (integer? x)
         (>= x 0))))

(define value
  (lambda (aexp)
    (cond ((natnum? aexp) aexp)
          ((@-exp? aexp)
           (@ (value (first-operand aexp))
              (value (second-operand aexp))))
          (($-exp? aexp)
           ($ (value (first-operand aexp))
              (value (second-operand aexp))))
          ((!-exp? aexp)
           (! (value (first-operand aexp))
              (value (second-operand aexp)))))))
;This code is said to proceed by recursive descent
;It is also described as syntax-directed

;What changes would we make if we wanted to pass @, $, and ! as parameters?

;One way: instead of the calls (value axep) occuring throughout, we could have
;(value aexp @ $ !) replacing each of these.

;Another way: define (val aexp) to be (value aexp @ $ !) and rewrite the value
;function given on the first board with val. Also this is the version in the notes
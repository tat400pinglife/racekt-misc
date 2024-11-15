;#lang eopl
; steps for question 1
; Task 1- design a datatype for propositions using at most and, or, implies, and not
;
; Datatypes generally have constructors, selectors, and classifiers
; for propositions in the class P
;        P is the least class containing
;          constants T,F     [noteL not #t, #f]
;          prop. values P,Q,R,S ...
;          which is closed under all the propositions
;
;  Construtors        Selectors         Classifiers
;  -----------        ----------        ------------
;   make-and           first-arg          and-prop?
;   make-or            second-arg         or-prop?
;   make-implies       operator           implies-prop?
;   make-not                              not-prop?

; Next steps: Assess the proposed datatype: will it be adequate to serve as an abstraction barrier?
;             (recall the process for the aexp exmaple)
;             After settling on a particular low-level representation of propositions we should
;             implement the functions comprising the datatypes.
;
; One way to assess the proposed datatype is to try to develop some abstract programs for
; accomplishing the tasks we're interested in.
;
; For exmaple -- we recall that DeMorgan's law permits transformation of:
;           not(P and Q)
;                to
;           (not p) or (not Q)
;
; Do we have enough in our proposed DS to do this?
; pre: input is not(P and Q)
; post: output is (not p) or (not Q)
;
;
;(define demorgan-1 
;  (lambda (prop)
;     (let* ((not-arg (first-arg prop))
;            (P (first-arg not-arg))
;            (Q (second-arg nor-arg))
;            (~P (make-not P))
;            (~Q (make-not Q)))
;           (make-or ~P ~Q))
;))
;
; this is just a layout without defined functions

; Hint for #1
; To show that any prop the the form (not A) or (not B), where A and B can contain the implies and 'and', to a prop
; where there are no occurences of implies or and
; We could:
;          - apply demorgan-2 to get not (A and B) from (not A) or (not B)
;          - invoke the recursion on A and on B to get logically equivalent A' and B'
;            where A' and B' don't contain implies and or
;          - So we could return not(A' and B')

; A possible concrete (low-level) representation of propositions might be as infix lists. Assumed P and Q are correct representations
;
; (define make-and
;   (lambda (P Q) (list P 'and Q)))
;
; the same goes for the other propositions
;
; (define first-arg
;   (lambda (p) (car p)))
;
; where (cadr p) is the propositions and (caddr p) is the second arg
;
; (define and-prop?
;   (lambda (p) (eq? (operator P) 'and)))
;
; the same goes for other propositions

;---------------------------------------------------------------------------

; part 2, what about a datastructure for (elementary) set theory?

; start: what operations?
;        we would seem to want basic operations such as:
;           . union-set
;           . intersect-set -------> should this op be in the data structure or is there an
;           . equal-sets?            underlying data structure which could be used to express
;           . empty-sets?            this in more general terms?
;           . make-empty-set?
;           . make-singleton-set?
;           . element-of-set?
;           . remove-elt-set
;           . subset?
;           . difference-set?
;           . product-set?
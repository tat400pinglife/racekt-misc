; class 10/31 notes

; base N bignum representation of numbers

; This is basically given an integer what is the base notation given base N. Like binary but its not just base 2. 

; Example: N=15
; (16_10) = 1*15^0 + 1*15^1 = (1 1)

; (242_10) = 2*15^0 + 1*15^1 + 1*15^2 = (2 1 1)

; (0) = ()

; then note that the output list can have elements that are as high as mod N or as low as 0.

; what is base 10 for (14 1), 14 + 15 = 29
; How can successors be computed using these big nums?
; What is the successor its 243 or in big nums:

; (2 1 1) -> (3 1 1), Is this a sufficiently general example?

; NO, what if the element is the max value of N - 1, like (14 1)?

; well it carries over to the second element -> (0 2)

; With this in mind what will the code look like?

; say you are given the values 16 and base 3, what is the first step?
; if 16/3^0 is > N-1, then check next for 16/N^1 where if that is still greater than keep going until it isnt
; where then you would return the remainder to one less than the power divided by
; then this should be a recursive call where first occurance will be like (if bignum/N^0 > N-1) then (bigint bignum (power N 2))
; ofcourse the output is a list so there needs to be cons some where like (cons n^k n^k+1)

; 16 base 3 = (1 2 1)= 1 + 6 + 9
; well lets think about this again 16/3 = R1, 15/9 = R6, 9/27 = R9
; then (cons ((remainder (bignum N) (bigint (- bignum remainder) (* N 


(define (succBase n)
  (letrec((succ (lambda (m)
                   (cond ((iszero? m) '(1))
                         ((= (- n 1) (car m)) (cons 0 (succ (cdr m))))
                         (else (cons (+ (car m) 1) (cdr m)))))))
    succ))

;-----------------------------------------------------------------------------------------------------------------

; okay now my personal attempt to create code that creates a representation of a positive integer in base N.
; with accompnaying functions pred and succ that will returns what the name says.


; ealier in this note I planned the representation to be a form of recursive quotient and subtraction like the function in earlier homework
; that requried analysis of each digit in a number.

; 16 in base 3 = (remiander of 16/3 = 1, remainder of 5/3 = 2, 1/3 = 1) basically the quotient gets passed to next call and the remainder gets cons.

; Why this works?

; well dividing a number gives us a representation of how the number can be split into powers. In this case it will be powers of
; N^p and N^p+1. Where it will start at p=0 and increase until the quotient is zero. Once it hits quotient of zero we know that further
; iterations of N^p+1 will not fit into our given positive integer.


; pre: x is a positive integer, y is a positive integer (not 0)

(define bigint
  (lambda (x y)
    (cond ((= x 0) '())
          (else (cons (remainder x y) (bigint (quotient x y) y)))

          )))
; post: bigint representation of x in power of y

; we know and have discussed the method of subtracting and borrowing from base 10 exmaples where if a value is zero it borrows from higher powers
; ex: (pred (0 2 2) 3) = (1 1 2), or (pred (0 0 1) 10) = (9 9 0)
; we can see a pattern where the new representation will be recursively borrow until it is complete

; now there are some edge cases where this doesn't really work out
; ex (pred ()) = ???, in this case the succesor will be negative and I dont like that
; in this case it will be the base case where bigint representation is 0

; What will the function need to operate?
; well it will need the power and the integer, this can be given directly where it will calculate the bigint representation
; or we can give it the bigint representation and the power directly

; now how would our code look like?
; 100% will be recusive call that will cdr down the representation
; It will also need to check for two cases:

; case 1: (car x) is not zero = easy case, just (cons (- (car x) 1) (pred (cdr x) y))
; AFTER TESTING, what if the last digit is 1 and it gets borrowed from?
; the 1 would become a zero and then it would need to be excluded

; case 2: (car x) is zero = more work, we know that it will be (cons (- y 1) (pred (cdr x) y))

; case 3: x is 1 and needs to return '()
(define 1k
  (bigint 1000 16))

(define test
  (bigint 16 3))

(define test1
  (bigint 0 3))

(define test2
  (bigint 54 3))

; pre: x is bigint representation, y is power
(define pred
  (lambda (x y)
    (cond ((null? x) '())
          ((= (car x) 0) (cons (- y 1) (pred (cdr x) y)))
          (else (if (and (= (car x) 1)(null? (cdr x))) '() (cons (- (car x) 1) (cdr x)))) ; why did i use' not' here, i was too lazy to move stuff around

          )))
; post: succ of x or (bigint (- x 1))


; well there is a bug for length 2 lists of (0 N-1)
; it should return (n-1 n-2), but returns (n-1)
; well there is an easier implementation but is very inefficient
(define pred_easy
  (lambda (x y)
    (bigint (- x 1) y)))


; now for succ

; We know this is the opposite of succ where it borrows from higher powers, now it carries over if it is larger than power N
; ex: (succ (2 1 1) 3) = (0 2 1), (succ (0 1 1) 3) = (1 1 1)
; This is like integer addition of base 10 where if the number added goes above 10 then it carries as a +1 for the higher power

; what should the code look like?

; it will be recursive and look for 2 main cases and the edge case where the bigint representation is ( (- y 1) (- y 1) ...)
; for the edge case the length of representation will increase by one
; ex: (succ (2 2 2) 3) = (0 0 0 1)

; case 1: (car x) = (- y 1) = more work, (cons 0 (succ (cdr x) y))
; this case will need a catch at the end where if (cdr x) is null then '(1) ;;; OKAY after thought is it 1 or '(1)

; case 2: (car x) != (- y 1) = easy, (cons (+ (car x) 1) (cdr x))
; ON SECOND THOUGHT, we dont even need to check for this condition as bigint will do it for us
; if case 1 is not true and we know that bigint only has numbers of modulo y meaning (0, 1, 2, 3, ... y-1)
; then it is 100% that (car x) is not (- y 1)

(define edge
  (bigint (+ 2 6 18 54) 3))

; pre: x is bigint representation, y is power
(define succ
  (lambda (x y)
    (cond ((null? x) '(1))
          ((= (- y 1) (car x)) (cons 0 (succ (cdr x) y)))
          (else (cons(+ (car x) 1) (cdr x)))
          )))
; post: successor of x

; again there is an easier implementation which is the same thing as in pred

; okay I just read that we need zero and iszero?
; so
(define zerotest
  '(1))

(define zero
  '())

(define iszero?
  (lambda (x)
    (eq? x zero)))
; is this not just (zero? x)
; no it is for our own numeric structure

; OKAY now given all this bigint stuff calculate factorial 10...

(define fact
  (lambda (x)
    (cond ((= x 0) 1)
          (else (* x (fact (- x 1))))
          )))

(define factorial
  (lambda (x n)
    (cond ((iszero? x) (succ zero n))
          ((eq? (succ zero n) x) '(1))
          (else (mult x (factorial (pred x n) n) n))
          )))


(define plus
  (lambda (x y n)
    (cond ((iszero? x) y)
          (else (succ (plus (pred x n) y n) n))))
  )


(define mult
  (lambda (x y n) ; x amount of y
    (cond ((iszero? x) zero)
          ((eq? '(1) x) y)
          (else (plus y (mult (pred x n) y n) n))
          )))
; this will be the succ of the succ of the succ until it reaches 0
; like incrementing y x times


(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define sum
  (lambda (x y)
    (cond ((zero? x) y)
          (else (add1 (sum (sub1 x) y))))))

; thats great that we have the factorial in our created number system, but can we turn it back?

(define normalize
  (lambda (x n)
   (define rev
    (lambda (x n pow)
      (cond ((null? x) 0)
            (else (+ (* (car x) (expt n pow)) (rev (cdr x) n (+ pow 1)))))))
    (rev x n 0)
    ))

; WITH THIS WE HAVE COMPLETED THE IMPLEMENTATION
; what is finished: input a number and power to create new number structure, can add and multiply using new number structure,
;                   can take factorial of new number structure, can normalize new number back to base 10.

; Limitations of code..., computing factorials of 10 seems to be the max without running out of memory
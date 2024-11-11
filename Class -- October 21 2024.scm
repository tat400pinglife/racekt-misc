

;; here is the statement of the problem we considered today in class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; balanced sequences of parentheses

; for this problem, we will think of 0 as a convenient substitute for a left bracket,
; and 1 as a convenient substitute for a right bracket. 

; a sequence of 0s and 1s is said to be balanced if (i) every 0 is later
; closed by some 1, and (ii) every 1 closes a previous 0.

; thus ((0 1)) is the list of all balanced sequences of length 2, and
; ( (0 0 1 1) (0 1 0 1) ) is the list of all balanced sequences of
; length 4

; Assuming n is a positive even integer, write and prove correct
; a function bal so that (bal n) returns a list - without duplicates -
; of all balanced sequences of 0s and 1s of length n.  Your program
; should be as efficient as you can make it: in particular, it should not generate
; any unbalanced sequences in the course of computing the final result.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; here is some code, without proof or even specifications, following the development given in class.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; questions for you:

;;  does it return only balanced binary lists?

; yes it does since it follows our balanced definition where it states that all wraps of balanced strings are balanced\
; and the concatenation of two balanced strings will give us a balanced string

;;  does it return all balanced binary lists of length n?

; yes it builds the final list from all lists of length n-2k where each string is concatenated with another string of length n-s, and the wrap of every previous balanced string



;;  can you prove its correctness?

; our BNF defines that all balanced strings are in the form (|bal|) and |bal bal|
; since our design idea reflects the idea that we output every single variation of this it is correct

;;  does it in fact return a duplicate-free list?  you can check (for example) that

;;    (= (length (bal 8)) (length (removeduplicates (bal 8))))

;; returns #t, but this is of course not a proof.

; If there was no method to remove duplicates we will indeed have some since the (EP) string of two concated strings will be the same resulting in dupes
; ex 0101 and 01 = (01)(0101) or (0101)(01)


;; can you see how to use bal (and perhaps a function union-sets) to compute the list of all balanced binary lists
;; of length <= n?


;; why is flatten needed?

; given that we recusively compute the different combinations of (s k), s+k=n, we will get list of list of lists of the different pairs
; instead of one list of lists


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bal n)
  (cond ((= 1 (modulo n 2))'())
        ((= n 0) '())
        ((= n 2) '((0 1)))
        (else (append (wrap (bal (- n 2))) (allConcats n)))))
; recursively go through all length n-2k


; given all balanced strings of length n-2 wrap then with parentheses
; mapping this procudre of list of lists n-2
(define (wrap listOfLists)
  (map (lambda (lst) (append (list 0) lst (list 1))) listOfLists))



(define (allConcats n)
  (removeDuplicates
   (flatten
    (map (lambda (pr)
           (let ((p (car pr))
                 (q (cadr pr)))
             (product (bal p) (bal q))))
         (nonTrivialEvenPartitions n)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))


(define (flatten lst)
  (accumulate append '() lst))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (elementOf? e l)
  (cond ((null? l) #f)
        ((equal? e (car l)) #t)
        (else (elementOf? e (cdr l)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (removeDuplicates lst)

  (cond ((null? lst) '())
        ((elementOf? (car lst) (cdr lst)) (removeDuplicates (cdr lst)))
        (else (cons (car lst) (removeDuplicates (cdr lst)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lst1 and lst2 are lists of lists

(define (product lst1 lst2)
  (flatten
   (map (lambda (l1)
          (map (lambda (l2) (append l1 l2))
               lst2))
        lst1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nonTrivialEvenPartitions n)
  
  (define (neitherIsZero p q)
    (and (not (zero? p)) (not (zero? q))))
  
  (define (bothAreEven p q)
    (and (even? p) (even? q)))
  
  (define (sumIs n p q)
    (= n (+ p q)))
  
  (define (enumerate-interval a b)
    (cond ((> a b) '())
          (else (cons a (enumerate-interval (+ a 1) b)))))

  (define (allPairs n)
    (flatten
     (map (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 n)))
          (enumerate-interval 1 n))))

  ; body of nonTrivialEvenPartitions
  (filter (lambda (pr)
            (let ((p (car pr))
                  (q (cadr pr)))
              (and (neitherIsZero p q) (bothAreEven p q) (sumIs n p q))))
          (allPairs n))
  )


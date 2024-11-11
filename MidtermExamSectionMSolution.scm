;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code and Development/Proof Indication for the Section M Midterm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Many proofs are omitted - talk to me in office hours if you need assistance
;; with the details. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some helper functions

;; n is a non-negative integer
(define (num-digits n)
  (cond ((< n 10) 1)
        (else (+ 1 (num-digits (/ n 10))))))


;; p and q are non-negative integers
;; returns [p][q] (as defined in class)
(define (make-number p q)
  (+ (* p (expt 10 (num-digits q))) q))

;; another helper, just for readability

;; n is a non-negative integer
;; returns rightmost digit of n
(define (rd n)
  (modulo n 10))


;; n >= 0 is an integer

;; the index of a digit d in n is the power of 10 corresponding to its position in n.  Thus in 1234, 4 has index 0.

;; returns the number formed by taking the even-indexed digits of n, in order
(define (number-formed-from-digits-with-even-indices n)
  (cond ((< n 10) n)
        (else (+ (* (number-formed-from-digits-with-even-indices (quotient n 100)) 10) (modulo n 10)))))

;; need to show that the even indexed digits of (quotient n 100) are all even indexed digits of n, except for (modulo n 10)

;; returns the number formed by taking the even-indexed digits of n, in order
(define (number-formed-from-digits-with-odd-indices n)
  (cond ((< n 100) (modulo (quotient n 10) 10))
        (else (+ (* (number-formed-from-digits-with-odd-indices (quotient n 100)) 10) (modulo (quotient n 10) 10)))))

;; need to show that the ODD indexed digits of (quotient n 100) are all ODD indexed digits of n, except for (modulo (quotient n 10) 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the primary helper function, merge


;; GI :  m = [m-nyp][m-ap] and m, m-nyp and m-ap are sorted
;;       n = [n-nyp][n-ap] and n, n-nyp and n-ap are sorted
;;       rsf is the sorted merge of m-ap and n-ap

;;       &&

;;       any digit in m-nyp is <= any digit in rsf

;;       &&

;;       any digit in n-nyp is <= any digit in rsf


;;; After some thought, it seems that it is necessary to specify that the sorted merge of a number n
;;; with 0 is n, and that the sorted merge of 0 with n is also n.  Thus the sorted merge of m-ap and
;;; n-ap is m-ap if (for example) it turns out that progress is not made on n for the first few steps.

;; Also: let me define [0][n] = n

;; Here is the code -- you'll note that this is the (unimproved, but still correct) solution to Quiz 2 in Section M.

;; m, m-nyp, m-ap, n, n-nyp, n-ap and rsf as above.  GI as above.  Spec as above.


(define (merge m n)
  (if (>= (rd m)(rd n))
      (merge-iter (quotient m 10) (rd m) n 0 (rd m))
      (merge-iter m 0 (quotient n 10) (rd n) (rd n))))
     

(define (merge-iter m-nyp m-ap n-nyp n-ap rsf)
  (cond ((zero? m-nyp) (make-number n-nyp rsf))
        ((zero? n-nyp) (make-number m-nyp rsf))
        
        (else (let ((rm-nyp (modulo m-nyp 10))
                    (rn-nyp (modulo n-nyp 10)))
                
                (cond ((>= rm-nyp rn-nyp)
                       (merge-iter (quotient m-nyp 10) (make-number rm-nyp m-ap)
                                   n-nyp n-ap
                                   (make-number rm-nyp rsf)))
                                 
                      (else
                       (merge-iter m-nyp m-ap
                                   (quotient n-nyp 10)(make-number rn-nyp n-ap)
                                   (make-number rn-nyp rsf))))))))
           
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the main function

;; n >= 0 is an integer

;; induction on the number of digits in n

;; merge is proved above

(define (mergesort n)
  (cond ((< n 10) n)
        (else
         (let ((even-indexed (number-formed-from-digits-with-even-indices n))
               (odd-indexed (number-formed-from-digits-with-odd-indices n)))
           (merge (mergesort even-indexed) (mergesort odd-indexed))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    


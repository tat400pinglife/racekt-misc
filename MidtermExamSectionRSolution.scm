


;; setting up the partition


;; The function all-digits-less, called as (all-digits-less n d), inputs

;;                     a nonnegative integer n which contains no 0s

;;                     a digit d

;; and returns any number formed from those digits of n which are less than d, with the same multiplicity as these occur in n, IF
;; there are such digits in n, and -1 otherwise



;; The function all-digits-greater-equal, called as (all-digits-greater-equal n d), inputs

;;                     a nonnegative integer n which contains no 0s

;;                     a digit d

;; and returns any number formed from those digits of n which are greater than or equal to d, with the same multiplicity as these occur in n, IF
;; there are such digits in n, and -1 otherwise


;; the problem specification can be changed if you wish to allow 0s, to reflect the fact that scheme does not allow leading 0s in the returned
;; value.  Check all the helpers if you want to do this. 

 
(define (all-digits-less n d)

  (define (update rsf d)
    (cond ((= rsf -1) d)
          (else (+ (* rsf 10) d))))

  ;; GI for iter:
  
  ;; n = [nyp][ap] &&
  
  ;; rsf = -1 iff no digits < d occur in ap
  ;; rsf <> -1 implies rsf is a number formed from all of the digits of ap which are less than d, with the same multiplicity as
  ;;           these occur in ap

  (define (iter n rsf)
    (cond ((< n 10) (cond ((< n d) (update rsf n))
                          (else rsf)))
          ((< (modulo n 10) d) (iter (quotient n 10) (update rsf (modulo n 10))))          
          (else (iter (quotient n 10) rsf))))


  (iter n -1))


(define (all-digits-greater-equal n d)

  (define (update rsf d)
    (cond ((= rsf -1) d)
          (else (+ (* rsf 10) d))))

  ;; GI for iter:
  
  ;; n = [nyp][ap] &&
  
  ;; rsf = -1 iff no digits >= d occur in ap
  ;; rsf <> -1 implies rsf is a number formed from all of the digits of ap which are >= d, with the same multiplicity as
  ;;           these occur in ap

  (define (iter n rsf)
    (cond ((< n 10) (cond ((>= n d) (update rsf n))
                          (else rsf)))
          ((>= (modulo n 10) d) (iter (quotient n 10) (update rsf (modulo n 10))))          
          (else (iter (quotient n 10) rsf))))


  (iter n -1))
    

;; two familiar helpers

;; n is a non-negative integer
(define (num-digits n)
  (cond ((< n 10) 1)
        (else (+ 1 (num-digits (/ n 10))))))


;; p and q are non-negative integers
;; returns [p][q]
(define (make-number p q)
  (+ (* p (expt 10 (num-digits q))) q))



;; the main function

;; designed and proved via induction on the number of digits in n

(define (quicksort n)
  (cond ((< n 10) n)
        (else (let ((digits-less (all-digits-less n (modulo n 10)))
                    (digits-greater-equal (all-digits-greater-equal n (modulo n 10))))
                (cond ((= digits-less -1) ;; so there are no digits smaller than the rightmost digit
                       (make-number (modulo n 10) (quicksort (quotient n 10))))

                      (else (make-number (quicksort digits-less) (quicksort digits-greater-equal))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;^^^^^^^^^^^^^^^^^^^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; of course there is at least one digit which is >=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (modulo n 10), so this cannot be -1.  So you have
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; the termination argument easily enough.  





                    



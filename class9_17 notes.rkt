; 9/17 class notes

;iteration
;We have looked at
(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))
;pattern of calls expansion shows how this process unfolds
; (fact 5)
; (* 5 (fact 4))
; (* 5 (* 4 (fact 3)))
; (* 5 (* 4(* 3 (fact 2))))
; ... until we get to the final recursion (fact 0) that returns 1
; (* 5 (* 4 (* 3 (* 2 1))))

;you can see that it requires O(n)space - on the stack - as well as O(n) time
;This is not an iterative program but probably recursive

;What is an iterative (or tail-recursive version)?
;This will work in constant space, meaning this will work without a stack
(define new-fact
  (lambda(n)
  (fact-iter 0 1 n)))
(define fact-iter
  (lambda (count rsf n)
    (cond ((= count n) rsf)
          (else
           (fact-iter (+ count 1)
                      (* (+ count 1) rsf)
                      n)))))
;Now lets view the calls expansion of the new factorial process
;  count  |  n  |  rsf
;    0       5      1
;    1       5      1
;    2       5      2
;    3       5      6
;    4       5      24
;    5       5     120

;How can we make this now efficient?
;We can combine the functions to reduce the amount of parameters passed per iteration
(define new-new-fact
  (lambda (x)
    (define new-fact-iter
             (lambda (count rsf)
               (cond((= count n) rsf)
                    (else(new-fact-iter (+ count 1)
                                        (* (+ count 1) rsf))
                    ))))
    (new-fact-iter 0 1)))

;Now can we do the same for our fibonacci program
;note for self replace the (fibe-iter 1 0 1 n) in the driver function to 1 1 2 n
;also make the driver fucntion internal

;old fib functio
;




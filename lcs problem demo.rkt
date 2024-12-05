; project part 2

; Given a 2 sequences output the longest common sub-sequence ( number and string ).


; note that we need two functions, first is the classical recursive exponential time
; The second is m*n time where it can use vectors and lists.

; note that vectors have limitations, but it has a faster look-up

; The classical recursive can be improved without using tables...

(define s1
  "abcdefghi")

(define s2
  "zedbcdefih")

(define s3
  "hello world")

(define s4
  "goodbye world")

(define s5
  "abcdeb")

(define s6
  "ecdaba")

(define s7
  "abcd")

(define s8
  "bcda")

(define lcs
  (lambda (s1 s2)
    (define help
      (lambda (s1 s2 lcs l)
        (cond
          ;; Base case: if either string is empty, return the current length and LCS
          ((or (null? s1) (null? s2))
           (cons l lcs))

          ;; If characters match, add to the LCS and continue with the rest
          ((eq? (car s1) (car s2))
           (help (cdr s1) (cdr s2) (append lcs (list (car s1))) (+ l 1)))

          ;; If characters don’t match, explore both possibilities
          (else
           (let* ((result1 (help (cdr s1) s2 lcs l))
                  (result2 (help s1 (cdr s2) lcs l)))
             (if (> (car result1) (car result2))
                 result1
                 result2))))))
    
    ;; Call the helper function and get the result
    (let ((result (help (string->list s1) (string->list s2) '() 0)))
      ;; Display the LCS and its length
      (display result)
      (display "LCS: ")
      (display (list->string (cdr result)))
      (newline)
      (display "Length: ")
      (display (car result))
      (newline))))


(define lcs2
  (lambda (s1 s2)
    (let ((memo '())) ;; Initialize memoization table as an empty alist
      ;; Memoized helper function
      (define (lcs-helper i j)
        (cond
         ;; Base case: If either string is empty, LCS is empty
         ((or (= i 0) (= j 0)) "")
         ;; Check if the result is already computed
         ((assoc (list i j) memo)
          (cdr (assoc (list i j) memo)))
         ;; If characters match, add to LCS and recurse
         ((char=? (string-ref s1 (- i 1)) (string-ref s2 (- j 1)))
          (let ((result (string-append
                         (lcs-helper (- i 1) (- j 1))
                         (string (string-ref s1 (- i 1))))))
            (set! memo (cons (cons (list i j) result) memo))
            result))
         ;; If characters do not match, take the max of two possibilities
         (else
          (let ((result (let ((option1 (lcs-helper (- i 1) j))
                              (option2 (lcs-helper i (- j 1))))
                          (if (> (string-length option1) (string-length option2))
                              option1
                              option2))))
            (set! memo (cons (cons (list i j) result) memo))
            result))))
      ;; Call helper with full lengths of strings
      (lcs-helper (string-length s1) (string-length s2)))))

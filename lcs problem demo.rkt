; project part 2

; Given a 2 sequences output the longest common sub-sequence ( number and string ).

(define s1
  "abcdefghi")

(define s2
  "zedbcdefih")

(define s3
  "hello world")

(define s4
  "goodbye world")

(define s5
  "abcd")

(define s6
  "bcda")

(define lcs
  (lambda (s1 s2)
   (define help
    (lambda (s1 s2 rsf l)
      (cond ((or (null? s1) (null? s2))  l)
            ((eq? (car s1) (car s2)) (help (cdr s1) (cdr s2) (append rsf (list(car s1))) (+ l 1)))
            (else (max (help (cdr s1) s2 rsf l) (help s1 (cdr s2) rsf l)))
            )))
    (help (string->list s1) (string->list s2) '() 0)
    )
            
  )

(define lcs2
  (lambda (s1 s2)
    (let* ((len1 (string-length s1))
           (len2 (string-length s2))
           (dp (make-vector (+ len1 1))))
      ;; Initialize the DP table
      (let loop1 ((i 0))
        (if (<= i len1)
            (begin
              (vector-set! dp i (make-vector (+ len2 1) 0))
              (loop1 (+ i 1)))))
      
      ;; Fill the DP table
      (let loop2 ((i 1))
        (if (<= i len1)
            (begin
              (let loop3 ((j 1))
                (if (<= j len2)
                    (begin
                      (let ((char1 (string-ref s1 (- i 1)))
                            (char2 (string-ref s2 (- j 1))))
                        (vector-set! (vector-ref dp i) j
                                     (if (char=? char1 char2)
                                         (+ 1 (vector-ref (vector-ref dp (- i 1)) (- j 1)))
                                         (max (vector-ref (vector-ref dp (- i 1)) j)
                                              (vector-ref (vector-ref dp i) (- j 1))))))
                      (loop3 (+ j 1)))))
              (loop2 (+ i 1)))))
      
      ;; Function to backtrack and find one LCS
      (define backtrack
        (lambda (i j)
          (if (or (= i 0) (= j 0))
              ""
              (let ((char1 (string-ref s1 (- i 1)))
                    (char2 (string-ref s2 (- j 1))))
                (if (char=? char1 char2)
                    (string-append (backtrack (- i 1) (- j 1)) (string char1))
                    (if (> (vector-ref (vector-ref dp (- i 1)) j)
                           (vector-ref (vector-ref dp i) (- j 1)))
                        (backtrack (- i 1) j)
                        (backtrack i (- j 1))))))))
      
      ;; Return the DP table and one LCS
      (list dp (backtrack len1 len2)))))

(define lcs3
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

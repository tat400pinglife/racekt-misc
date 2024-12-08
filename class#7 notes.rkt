;class #7 notes

; another bnf def. exercise
;
; BNF def. of the class of balanced strings of parenthesis
;
; basically correct closing parentheses eg; (), (()) not ())(, )(
;
; bal::= empty or |(bal)|, meaning you can wrap up a balanced string
;    ::= bal*bal, you can concatentate a balanced string
;
; Parens are awkward for scheme so lets adapt to balanced lists of 0s and 1s.
; The idea: 0 will stand for a left paren, 1 will stand for a right paren
;
; That is bal::= ()|(0 bal 1)|(append bal bal)
;
; Goal today: Develop an R5RS program which inputs an even integer n >= 2 and returns the a set of all elements of bal <= n elements
;
; example with n=4: (0101) concat of (01) (01)
;                   (0011) wrap of (01)
; so output will be ((0101) (0011))
;
; rejecting the idea of producing all binary lists of length <= n and filtering for the balanced property (exponential intermediate storage),
; we opt for a design that uses the BNF def. directly
;
; A divide and conquer strategy might be to form the union of - all wraps of (balbin (- n 2))
;                                                             - all concatenations of even partitions of shorter balanced lists
; we note that a list obtained as a concatenation cannot be obtained as a wrap
; I think that remaining duplicates will be more efficient if this can be done before the union is computed
;
; note to self for test- don't spend too much time coding out helper funcitons
;
; HELPER FUNCTIONS
;------------------
; pre: a list of sets
; post: return the union of these
; (union-sets s1 s2 s3 ... sk)
;
; Just an outline is okay
;
; pre: a list L of 0s and 1s
; post: (0 L 1)
; (wrap L)
;
; pre: even integer n>= 2
; post: lists of partitiions of n
; (partitions n)
;
; pre: n>= 2 n is even
; post: returns set of all ball.bin lists of length n
; (balbin n)
;
; pre: 2 lists L1 and L2 of lists
; post: all lists which can be formed by appending (car L1) and each elt of L2, (cadr L1) and each elt of L2
; (concat L1 L2) , where we are assuming that there are no duplicates in  L1 and L2
;
; (define flatten
;    (lambda (lst-of-lsts)
;       (accumulate append '() list-of-lsts)))
;
; Is the proposed listing of partitions of n convenient for our goal?
; if n = 10, then (partitions n) = ((2 8) (4 6) (6 4) (8 2))                                   here we run into a problem, we want a list of balanced sequences
; we want to replace each pair (i j)by ((balbin i) (balbin j)) - for (4 6), for example,       not a lists of lists of balanced sequences, looks liek a job for flatten
; we'd have ((0011) (0101)) = (balbin 4)                                                       flatten in obtained from accumulate and append
;           ((000111) (001011) (010011) (010101) (001101) (010101)) = (balbin 6) 
;                    wrap                       concat
; we can observe that concat gives us a duplicate, if so, can we say that there are at most n-1 different strings
; From these we can obtain lists of length 10 by using something like a set-product
;
; After you have a solid first cut at your helper functions, try to formulate the top level program
;
; pre: will n>= 0 work?    , here we see that an improvement of our pre condition as 0 returns an empty list
(define balbin
  (lambda (n)
    (cond ((zero? n) (list '()))
          (else
           (union-set
            (wraps (balbin(- n 2)))
            (concats ... some mapping operation of the concat
))))))

; struct. induction: by the inductive hypothesis that every 0 has a 1 to complete the wrap
; will not affect the balanced string, same for concat where each left and right string is already balanced
; REMEMBER that language is neccesary and should match the definition given
; like every 0 has a mate 1
;
; Now that the BNF definition is sound according to the definition given in question
; How can we prove that it is complete?
; If we make a balanced string according to the balanced properties is it included in our BNF definition
;
; what about completeness? is every balanced list a member of bal?
;
; An induction on the length of the list might do the job
;
; So how do we start the induction?
; with the base case!!! :))))) hahahahahha im going crazy
; what is the base case in this situation?
; is it that every string must have even length? can we even prove that? does the definition of balanced force even length?
;
; In the case in OH the defintion was just that 0 must be closed by 1 later on and 1 must be closed by 0 before
; the definition was missing the specificality that each paring must be unique so under the previous def.
; we can end up with (011)

;;;; not me but the prof gave this????
; --The basis, for () , is clear
; --if s is a balanced list, then perhaps we could look into decomposing s as
;   a concatenation s1 s2 in which every 0 in s1 is mated to a 1 in s1 and therefore
;   every 0 in s2 is amted to a 1 in s2
;
; -- and if no such decomposition exists, try to argue that s must be a wrap;
;
; try to finish this yourself
;;;; LIKE FUCK IF I CNA I WOULD BE IN OH




;
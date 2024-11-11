;class#4 notes

;pertaining for function scanning list for nil

;-> notice that the spec says nothing about how the computation is to proceed. we say that specs such as this are declarative (They say WHAT is to be computed but not how)
;The opposite is the case for imperative languages.
;
;pre: x is a list containing only atoms and '()
(define contains-nil?
  (lambda (x)
    (and ((null? x) #f)
         ((eq? (car x) '()) #t)
         (else (contains-nil? (cdr x))))))

;post: determines whether '() occurs in x

;This triple of pre, prog, post can be viewed informally as an implication:
;"if pre holds and if prog is run, then post is true of the result."

;-> in particular, the spec says nothing about *runtime notation*(prog(input); it doesn't say whether the computation is to be recursive or iterative.
;Thus, we can describe preconditions as strong or weak, depending on the number of possible worlds that they rule out.
;A strong pre is one that is very restrictive. For example, the precondition FALSE is extrememly strong-strongest possible- because there is no world in which FALSE is true.
;We get as customers, we want the weakest precondition.

;Please think about post conditions: Do we as programmers want strongest possbile or weakest possible conditions?
;Of course we want to have a strong precondition as the output should be precise.

;Design process:
;The first step is to determine the design process/idea.
;In this example above it can be : cdr down the list, inspective each element in turn and returning #t if/when '() is encountered as an element of x.

;Details on the design:
; -> check whether the current element is '(), (car x), Since the pre guarantees that elements of x are atoms or '(), the appropriate test will be eq?
; -> if (eq? (car x) '()), program returns #t
; -> otherwise, by the obvious IH ("if the pre holds when the rec call is made, the rec call returns the correct result") The prog should return what ever the recursive call returns.

;create a program that inputs x and an integer k >_ 0 and returns x without the kth occurence of '(), (This spec is flawed as there must be atleast k occurences of the value '())
;or a program that inputs x and return the index of the first occurrence of '() in x, keep in mind that scheme has a 
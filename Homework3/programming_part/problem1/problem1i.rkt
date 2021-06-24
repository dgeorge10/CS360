#lang racket

;PROBLEM 1

;1 i A
;assoc_list is a list of bindings.
;Goal: find binding (pair) whose name equals the given name
(define (lookup name assoc_list)
  (cond
    ;if assoc_list is null return a null list
    ((null? assoc_list) '()) 
    ;if the first element of the first binding is equal to name, return the binding
    ((eq? name (car (car assoc_list))) (car assoc_list))
    ;otherwise recursion, call the function again with the rest of the list of bindings
    (else (lookup name (cdr assoc_list)))))

(define assoc_list1 '( (dennis "george") (stephen "hansen") (shivanshi "nagar")))
(lookup 'dennis assoc_list1)
(lookup 'ben assoc_list1)

;1 i B
;Goal: find binding with the specified name in an environment
(define (lookup-env name environment)
  ;if environment is null
  (if (null? environment) '()
      ;look for each name using function lookup in each association list
      (let ((each (lookup name (car environment))))
        ;if found return the name
        (if (not (null? each)) each
            ;if not call the function with the rest of the environment 
            (lookup-env name (cdr environment))))))
  
(define env1 '( (sixers "philly") (lakers "la") (blazers "portland")))
(define env2 '( (chicken "chick fil a") (mint "chocolate") (subway "sandwiches") ))
(define env (list env1 env2) )
(lookup-env 'sixers env)
(lookup-env 'mint env)
(lookup-env 'hoagie env)








  
;;; Example 11.20 (Figures 11.1 and 11.2)

(define simulate
  (lambda (dfa input)
    (letrec ((helper  ; note that helper is tail recursive,
              ; but builds the list of moves in reverse order
              (lambda (moves d2 i)
                (let ((c (current-state d2)))
                  (if (null? i) (cons c moves)
                      (helper (cons c moves) (move d2 (car i)) (cdr i)))))))
      (let ((moves (helper '() dfa input)))
        (reverse (cons (if (is-final? (car moves) dfa)
                           'accept 'reject) moves))))))

;; access functions for machine description:
(define current-state car)
(define transition-function cadr)
(define final-states caddr)
(define is-final? (lambda (s dfa) (memq s (final-states dfa))))

(define move
  (lambda (dfa symbol)
    (let ((cs (current-state dfa)) (trans (transition-function dfa)))
      (list
       (if (eq? cs 'error)
           'error
           (let ((pair (assoc (list cs symbol) trans)))
             (if pair (cadr pair) 'error))) ; new start state
       trans                                ; same transition function
       (final-states dfa)))))               ; same final states

(define zero-one-even-dfa
 '(q0                                                 ; start state
   (((q0 0) q2) ((q0 1) q1) ((q1 0) q3) ((q1 1) q0)   ; transition fn
    ((q2 0) q0) ((q2 1) q3) ((q3 0) q1) ((q3 1) q2))
   (q0)))                                             ; final states

(define test1
  (lambda ()
    (simulate
     zero-one-even-dfa  ; machine description
     '(0 1 1 0 1))))    ; input string

(define test2
  (lambda ()
    (simulate
     zero-one-even-dfa  ; machine description
     '(0 1 0 0 1 0))))  ; input string


(display "FIGURE 3\n")
(define dfa-three
  '(q0
    (((q0 "a") q1) ((q0 "b") q2) ((q0 "c") q3)
    ((q1 "b") q4) ((q1 "a") q1) ((q1 "c") q3)
    ((q2 "b") q2) ((q2 "c") q5) ((q2 "a") q1)
    ((q3 "c") q3) ((q3 "a") q6) ((q3 "b") q2)
    ((q4 "a") q1) ((q4 "b") q2) ((q4 "c") q5)
    ((q5 "b") q2) ((q5 "c") q3) ((q5 "a") q6)
    ((q6 "a") q1) ((q6 "c") q3) ((q6 "b") q4))
    (q4 q5 q6)))

;accept
(define test3-accept
  (lambda ()
    (simulate
     dfa-three  ; machine description
     '("a" "b" "c")))); input string
(test3-accept)

(define test3-ii
  (lambda ()
    (simulate
     dfa-three  ; machine description
     '("b" "c" "a")))); input string
(test3-ii)

(define test3-iii
  (lambda ()
    (simulate
     dfa-three  ; machine description
     '("c" "a" "b")))); input string
(test3-iii)

(display "FIGURE 8 #1's odd\n")
(define dfa-eight-part-one
	'(q0
	(((q0 0) q0) ((q0 1) q1)
	((q1 0) q1) ((q1 1) q0))
	(q1)))

;accept
(define test8-1-accept
  (lambda ()
    (simulate
     dfa-eight-part-one  ; machine description
     '(1 0 1 1 1 0 1)))); input string
(test8-1-accept)

;reject
(define test8-1-reject
  (lambda ()
    (simulate
     dfa-eight-part-one  ; machine description
     '(1 0 1 1 0 1)))); input string
(test8-1-reject)

(display "FIGURE 8 #1's even\n")
(define dfa-eight-part-two
   '(q0
   (((q0 0) q0) ((q0 1) q1)
   ((q1 0) q1) ((q1 1) q0))
   (q0)))

;accept
(define test8-2-accept
  (lambda ()
    (simulate
     dfa-eight-part-two  ; machine description
     '(1 0 1 1 0 1)))); input string
(test8-2-accept)

;reject
(define test8-2-reject
  (lambda ()
    (simulate
     dfa-eight-part-two  ; machine description
     '(1 0 1 1 0 1 1)))); input string
(test8-2-reject)

(display "FIGURE 10")
(define dfa-ten
	'(q0
	(((q0 0) q0) ((q0 1) q1)
	((q1 0) q0) ((q1 1) q2)
	((q2 0) q2) ((q2 1) q2))
	(q0 q1)))

;accept
(define test10-accept
  (lambda ()
    (simulate
     dfa-ten
	  '(1 0 1 0 1 0))))
(test10-accept)

;reject
(define test10-reject
  (lambda ()
    (simulate
     dfa-ten
     '(1 0 1 1 0 1 0))))
(test10-reject)

(display "FIGURE 11-#0-#1-both-odd")
(define dfa-eleven-part-one
	'(q0
	(((q0 0) q1) ((q0 1) q3)
	((q1 0) q0) ((q1 1) q2)
	((q2 1) q1) ((q2 0) q3)
	((q3 0) q2) ((q3 1) q0))
	(q2)))

;accept
(define test11-1-accept
  (lambda ()
    (simulate
     dfa-eleven-part-one
	  '(1 1 0 0 1 0))))
(test11-1-accept)

;reject
(define test11-1-reject
  (lambda ()
    (simulate
     dfa-eleven-part-one
     '(1 1 1 0 1 0))))
(test11-1-reject)

(display "FIGURE 11-#0-#1-both-even")
(define dfa-eleven-part-two
   '(q0
   (((q0 0) q1) ((q0 1) q3)
   ((q1 0) q0) ((q1 1) q2)
   ((q2 1) q1) ((q2 0) q3)
   ((q3 0) q2) ((q3 1) q0))
   (q0)))

;accept
(define test11-2-accept
  (lambda ()
    (simulate
     dfa-eleven-part-two
	  '(1 1 0 0 1 0 0 1))))
(test11-2-accept)

;reject
(define test11-2-reject
  (lambda ()
    (simulate
     dfa-eleven-part-two
     '(1 1 0 0 0 1))))
(test11-2-reject)

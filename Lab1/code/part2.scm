;; 2i.

; Assuming n>=1 for these

; Basing style off of weeks 1-2 slides

; Non-tail recursive n!
(define factorial-notail
  (lambda (n)
	(if (= n 1)
		1
		(* n (factorial-notail (- n 1))))))

; Tail recursive n!
(define factorial-tail-helper
  (lambda (n result)
	(if (= n 1)
		result
		(factorial-tail-helper (- n 1) (* n result)))))

(define factorial-tail
  (lambda (n) (factorial-tail-helper n 1)))

;; 2ii.

; Assuming n>=0 for these

; Non-tail recursive 2^n
(define exponential-notail
  (lambda (n)
	(if (= n 0)
		1
		(* 2 (exponential-notail (- n 1))))))

; Tail recursive 2^n
(define exponential-tail-helper
  (lambda (n result)
	(if (= n 0)
		result
		(exponential-tail-helper (- n 1) (* 2 result)))))

(define exponential-tail
  (lambda (n) (exponential-tail-helper n 1)))

;; 2iii.
(define (compose g f) (lambda (x) (g (f x))))

(define exponential-factorial-notail
  (compose exponential-notail factorial-notail))

(define exponential-factorial-tail
  (compose exponential-tail factorial-tail))


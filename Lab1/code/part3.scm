;; 3i.
(define (range L)
  (let ((start (car L))
		(step (cadr L))
		(end (caddr L)))
	(if (> start end)
		'()
		(cons start (range (list (+ start step) step end))))))

;; 3ii.

; Very trivial to just run map on (range L) here
(define (seq f L)
  (map f (range L)))

; Alternatively if you want a non-map approach:
(define (seq-nomap f L)
  (let ((start (car L))
		(step (cadr L))
		(end (caddr L)))
	(if (> start end)
		'()
		(cons (f start) (seq-nomap f (list (+ start step) step end))))))


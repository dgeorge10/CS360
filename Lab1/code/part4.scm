;; 4.

(define (binom n k)
  (if (and (>= n 0) (>= k 0) (>= n k))
	  (if (or (= k 0) (= k n))
		  1
		  (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))
	  0))



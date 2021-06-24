;; 1i.

; Determine whether x is an element in list L.
;	If a list is empty, then x is not an element in L.
;	If the first element of a list is L, then x is an element in L.
;	Otherwise, check if x is a member of the remaining elements of L.
;

(define (member x L)
  (cond
    ((null? L) #f)
    ((equal? x (car L)) #t)
    (else (member x (cdr L)))))

; Insert an element x in list L if x is not already in L.
;	If x is a member of L, return the list itself L (identity function).
;	Otherwise, return a new list containing x as the first element and the
;	remaining elements being the elements of L.

(define (insert x L)
  (if (member x L) L
      (cons x L)))

;; 1ii.

; Find the maximum and minimum elements in a list L. Return a list
; whose first element is the max and the second element is the min.
;	If L is null, return an empty list (to handle errors).
;	If L is a single element (null cdr), return that element as both
;	the max and the min of the list L.
;	Otherwise, find the max and min of the rest of the list and
;	compare the max and min to the first element of the list.
;	If the first element is greater than the current max,
;	replace the current max with the first element.
;	If the first element is less than the current min,
;	replace the current min with the first element.
;	Otherwise return the maxmin list, unmodified by the first element.
;

(define (maxmin L)
  (cond
    ((null? L) '()) ;; empty list represents error
    ((null? (cdr L)) (list (car L) (car L))) ;; list of 2 items
    (else (let ((mmtemp (maxmin (cdr L)))
                (first (car L)))
            (cond
              ((> first (car mmtemp))
               (cons first (cdr mmtemp)))
              ((< first (car (cdr mmtemp)))
               (list (car mmtemp) first))
              (else mmtemp))))))

;; 1iii.

(define (merge fst snd)
  (cond
    ((null? fst) snd)
    ((null? snd) fst)
    (else
     (let ((x (car fst)) (y (car snd)))
       (if (< x y) (cons x (merge (cdr fst) snd))
           (cons y (merge fst (cdr snd))))))))

(define (split lis)
  (cond
    ((null? lis) (cons '() '()))
    ((null? (cdr lis)) (cons (list (car lis)) '()))
    (else
     (let ((a (car lis)) (b (car (cdr lis))) (c (split (cdr (cdr lis)))))
       (cons (cons a (car c)) (cons b (cdr c)))))))

; Given a list, run the mergesort algorithm on the list to sort the elements.
;	If the list is null, return the empty list.
;	If the list is a single element (null cdr), return the list itself.
;	Otherwise, split the list into two separate lists. Recursively run
;	mergesort on the first and second split lists to continue splitting them.
;	Call merge on the results of msort (the results being the split lists, sorted)
;	to merge the two lists back together into one sorted list.
;

(define (msort lis)
  (cond
    ((null? lis) '())
    ((null? (cdr lis)) lis)
    (else
     (let* ((c (split lis)) (fst (car c)) (snd (cdr c)))
       (merge (msort fst) (msort snd))))))


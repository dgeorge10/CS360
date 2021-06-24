;A Scheme Binary Search Tree
;Each node has a value and a left and right subtree
;Represented by a list (value left right)

;Return the Value of a node
(define (tree-value node)
	(car node)
)
;Return the left child of a node
(define (left-child node)
	(car (cdr node))
)
;Return the right child of a node
(define (right-child node)
	(car (cdr (cdr node)))
)

;Insert
;The insert function takes an item and inserts in into the binary tree.
;The empty list is also the empty binary tree
(define (insert item tree)
	(cond
		((null? tree) (list item '() '())) ;If the tree is null make a new one
		(( < item (tree-value tree)) ;Decide Right or Left Branch
			(list (tree-value tree)
				(insert item ( left-child tree)) (right-child tree)
			)
		)
		(else (list (tree-value tree) 
			(left-child tree) (insert item (right-child tree)))
		)
	)
)

;Member
;The member function returns true if an item is in the Binary Search Tree
(define (member item tree)
	(cond
		((null? tree) #f) ;It is not in the null tree
		((= item (tree-value tree)) #t) ; It is the current node value
		((< item (tree-value tree)) (member item (left-child tree)));If it exists it is on the left side
		(else (member item (right-child tree))); If it exists it is on the right
	)
)

;helper function to determine the height of a BST
(define bst-height 
  (lambda (bst)
	(cond 
	  ((null? bst) -1)
	  (else (+ 1 (max (bst-height (left-child bst))
			  (bst-height (right-child bst))))))))

(define BST '())
(define BST (insert 4 BST))
(define BST (insert 2 BST))
(define BST (insert 1 BST))
(define BST (insert 3 BST))
(define BST (insert 6 BST))
(define BST (insert 5 BST))
(define BST (insert 7 BST))


(member 1 BST)
(member 2 BST)
(member 3 BST)
(member 4 BST)
(member 5 BST)
(member 6 BST)
(member 7 BST)

;To see what BST looks like
(begin BST)

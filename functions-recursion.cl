(defun f2(x)
	(cond	((< x 2)	(* 2 x) )
			((<= x 10)	-1		)
			(t			(* x x) )
	)
)

(defun g (x y)
	(let
		(	
			(z 1)
			(z2 2)
		)
		(setf z (* x y) )
		(+ z 1)
	)
)

(defun price-change(old new)
	"Get price difference in percentage"
	(let*	(	(diff 		(- new old)   )
				(ratio 		(/ diff old)  )
				(pIncrease  (* ratio 100) )
			)
		(list 'value 'changed 'by pIncrease 'percent)
	)
)

(defun my-member (number list)
	"Find member of a list recursively"
	(cond	((null list) 				nil  							)
			((equal number (car list)) 	list 							)
			(t 							(my-member number (cdr list))	)
	)
)

(defun my-length (list)
	"Get length of list recursively"
	(cond 	((null list) 	nil 								)
			(t 				(+ 1 (my-length (cdr list))) 	)
	)
)

(defun my-nth (n list)
	"Get nth element of a list recursively"
	(cond	((= n 0) 			(car list)	 )
			((null list) 		nil			 )
			(t (my-nth (- n 1) 	(cdr list))	 )
	)
)

(defun filter-negs (list)
	"Recursively remove negatives from a list"
	(cond	((null list) 			nil				 							)
			((minusp (car list)) 	(filter-negs (cdr list)) 					)
			(t 						(cons (car list) (filter-negs (cdr list))) 	)
	)
)
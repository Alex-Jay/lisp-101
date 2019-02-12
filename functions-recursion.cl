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
			(t 				(+ 1 (my-length (cdr list))) 		)
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

(defun my-append (list1 list2)
	"Append two lists together recursively"
	(cond	((null list1) 			list2 												)
			(t 						(cons (car list1) (my-append (cdr list1) list2)) 	)
	)
)

(defun count-atoms (list)
	"Recursively count how many atoms exist in a list"
	(cond 	((null list) 		0 														)
			((atom list) 		1 														)
			(t 					(+ (count-atoms (car list)) (count-atoms (cdr list)))	)
	)
)

;;; (mapcar #'abs '(-4 -3 0 55 4.3 -120.33) )
;;; Applies abs function to all elements of the list

;;; (format t "Hello, World!")
;;; Prints text to screen

;;; (format t "List: ~S" '(1 2 3 4 5))
;;; Prints formatted list

;;; (format t "Hello, ~%~S" john)
;;; ~% adds new line

;;; (format t "~&~S")
;;; ~& adds new line if text already exists on current line

;;; (dotimes (i 4) (format t "~&i: ~S" i))
;;; Prints formatted text 4 times with new lines

;;; (dolist (i '(red green blue)) (format t "~&i: ~S" i) )
;;; Iterates a list and prints it in a formatted fashion
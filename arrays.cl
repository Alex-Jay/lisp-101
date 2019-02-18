(setf m1 (make-array '(3 2) :initial-contents '( (1 2) (-3 5) (7 12) )) )
(setf m2 (make-array '(2 4) :initial-contents '( (2 -4 5 8) (-1 -3.7 1.5 2) )) )

;;(format t "~&m1 = ~S" m1)
;;(format t "~&dim(m1) = ~S" (array-dimensions m1) )
;;(format t "~&m1 element 1 0 = ~S" (aref m1 1 0) )

;;(format t "~&dim(m2) = ~S" (array-dimensions m2) )
;;(format t "~&m2 element 0 3 = ~S" (aref m2 0 3) )
;;(format t "~&m2 element 1 2 = ~S" (aref m2 1 2) )

;; Replace element
;;(format t "~&m1 element 2 1 = ~S" (aref m1 2 1) )
;;(setf (aref m1 2 1) 9) 
;;(format t "~&m1 element 2 1 = ~S" (aref m1 2 1) )

;; Increment value at position by 3
;;(incf (aref m1 2 1) 3) 
;;(format t "~&m1 element 2 1 = ~S" (aref m1 2 1) )

;; Multiply 2 matricies together
;; Matrix a = m * s (dimensions)
;; Matrix b = s * n	(dimensions)
(defun mult (a b)
	(let* 	(
				(m (nth 0 (array-dimensions a)) )
				(s (nth 1 (array-dimensions a)) )
				(n (nth 1 (array-dimensions b)) )
				(result (make-array (list m n))	)
			)
	
	(dotimes (i m)
		(dotimes (j n)
			(setf (aref result i j) 0.0)
			(dotimes (k s)
				(incf (aref result i j) (* (aref a i k) (aref b k j)))
			)
		)
	)
	result
	)
)

(format t "~%~%~%m1: ~S" m1)
(format t "~%~%~%m2: ~S" m2)
(format t "~%~%~%Result: ~S" (mult m1 m2))

;; Neural Network functions

;; Pads NN Input Matrix with 1's
;; Resulting Matrix = m * (n + 1) [Dummy values needs extra column]
(defun prepend1 (a)
	
	(let* 	(
				(m (nth 0 (array-dimensions a)) 		)
				(n (nth 1 (array-dimensions a))			)
				(result (make-array (list m (+ n 1)))	)
			)
	
	;; Iterate rows
	(dotimes (i m)
		;; i(th) row in column 0
		(setf (aref result i 0) 1.0)
	)
	
	;; Iterate rows
	(dotimes (i m)
		;; Iterate columns
		(dotimes (j n)
			;; Set matrix values infront of padded 1's in column 0
			(setf (aref result i (+ 1 j)) (aref a i j))
		)
	)
	result
	)
	
)
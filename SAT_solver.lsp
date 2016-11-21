(defun num-member (li)	;RETURNS NUMBER OF MEMBERS IN LIST
	(cond	((null li)	0)
			(t	(+ 1 (num-member (cdr li))))
	)
)

(defun shortest-list1 (delta)	;RETURNS MEMBER LIST WITH SMALLEST NUM-MEMBER
	(cond	((null delta)	nil)
			((null (cdr delta))	(car delta))
			((< (num-member (car delta)) (num-member (shortest-list1 (cdr delta))))	(car delta))
			(t	(shortest-list1 (cdr delta)))
	)
)

(defun delete-second (li)	;delete the second element from the list
	(cond	((null li)	nil)
			((null (cdr li))	(list (car li)))
			((= 2 (num-member li))	(list (car li)))
			(t	(cons (car li) (cddr li)))
	)
)

(defun shortest-list2 (delta)	;same as shortest-list1: but I think this might be faster
	(cond	((null delta)	nil)
			((null (cdr delta))	(car delta))
			((> (num-member (car delta)) (num-member (second delta)))	(shortest-list2 (cdr delta)))
			(t	(shortest-list2 (delete-second delta)))
	)
)
;;;;;;;;;;;;;;which one is faster?

(defun exist (n li)		;check if n exists in list including negate of n
	(cond	((null li)	nil)
			((or (= n (car li)) (= (* -1 n) (car li)))	t)
			(t	(exist n (cdr li)))			
	)
)

(defun num-used (n delta)	;count how many times n being used
	(cond	((null delta) 0)
			((exist n (car delta))	(+ 1 (num-used n (cdr delta))))
			(t	(num-used n (cdr delta)))
	)
)

(defun most-used (li delta)		;return variable which is most used in delta among list 
	(cond	((null li)	nil)
			((null (cdr li))	(car li))
			((< (num-used (car li) delta) (num-used (second li) delta))	(most-used (cdr li) delta))
			(t	(most-used (delete-second li) delta))
	)
)



(defun delete-var (n li)	;delete var(n) from list(li)
	(cond	((null li)	nil)
			((= n (car li))	(cdr li))
			(t	(cons (car li) (delete-var n (cdr li))))			
	)
)

(defun exist-only-n (n li)	;check if n exists in list not including negate of n
	(cond	((null li)	nil)
			((equal n (car li))	t)
			(t	(exist-only-n n (cdr li)))			
	)
)

(defun delete-list-with-n (n delta)		;delete all the list which contains n from delta
	(cond	((null delta)	nil)
			((exist-only-n n (car delta))	(delete-list-with-n n (cdr delta)))
			(t	(cons (car delta) (delete-list-with-n n (cdr delta))))
	)
)

(defun delete-all-n (n delta)		;delete all n's from each list of delta
	(cond	((null delta)	nil)
			((exist-only-n n (car delta))	(cons (delete-var n (car delta)) (delete-all-n n (cdr delta))))
			(t	(cons (car delta) (delete-all-n n (cdr delta))))
	)
)


(defun exist-null (li)		;check if null exists in list
	(cond	((null li)	nil)
			((null (car li))	t)
			(t	(exist-null (cdr li)))			
	)
)

(defun if-unit-clause (delta)		;if delta has unit-cluase, return its unit-clause. otherwise, return nil
	(cond	((null delta)	nil)
			((= 1 (num-member (car delta)))	(car delta))
			(t	(if-unit-clause (cdr delta)))
	)
)

(defun num-check1 (n li)	;helper function of checking pure number
	(cond		((null li) 		nil)
				((= (* -1 n) 	(car li))	t)
				(t				(num-check1 n (cdr li)))	
	)
)
(defun num-check2 (n delta)		;return true if n is not pure
	(cond		((null delta) 		nil)
				((num-check1 n (car delta))		t)
				(t 								(num-check2 n (cdr delta)))
	)
)
(defun pure-literal (delta)		;from delta, return list of all the pure literals
	(cond	((null delta)			nil)
			((null (car delta))		(pure-literal (cdr delta)))
			((null (cdr delta))		(car delta))
			((num-check2 (caar delta) (cdr delta))	(pure-literal (delete-all-n (* -1 (caar delta)) (delete-all-n (caar delta) delta))));;;;;modify needed
			(t						(cons (caar delta) (pure-literal (delete-all-n (caar delta) delta))))
	)
)
(defun pure-elimination (li delta)		;delete all list containing pure-literals and returns resulted delta
	(cond	((null li)		delta)
			(t 				(pure-elimination (cdr li) (delete-list-with-n (car li) delta)))
	)
)

(defun unit-pure (delta var) ;returns nil or result;	nil when fails;		
;result is list of two components: 	first is list of assigned var;	 second delta ; if second is nil then it means it is satisfied
;basically, this function simplifies the delta by using unit propagation and pure literal elimination
;it is DPLL
	(let*	(
			(li		(if-unit-clause delta))
			)
			(cond	((not (null li))	;;first, unit propagation. Delete all the unit clauses and assign variables properly	
											(let*	(
													(n		(* -1 (car li)))
													(new-delta1		(delete-list-with-n (car li) delta))
													(new-delta2		(delete-all-n n new-delta1))
													(new-var		(cons (car li) var))
													)
													(cond	((exist-null new-delta2)		nil)
															(t	(unit-pure new-delta2 new-var))
													)
											)
					)
					(t		(cond	((exist-null delta)		nil)
									((null delta)		(cons var (list nil)))	;;second, do the pure literal elimination
									(t				(let*	(
															(pure 			(pure-literal delta))
															(new-delta3 	(pure-elimination pure delta))
															(new-var2		(append pure var))
															(result			(cons new-var2 (list new-delta3)))
															)
															result
													)									
									)
							)
					)
			)	
	)
)

;;
(defun complete-model (n li)	;n is number of argument and li is assigned variable so far. 
;Thus left variables are okay to be assigned any values
;So make complete model by assinging them "true" (number itself)
	(cond	((= n 0)	li)
			((exist n li) 			(complete-model (- n 1) li))
			(t						(complete-model (- n 1) (cons n li)))
	)
)


(defun sat1? (n delta path) ;helper function of sat?
;actually this is main function
;it takes one more argument than what it is actually supposed to have
	(let*	(
			(simple 	(unit-pure delta nil))	;first part of DPLL ;update it by using unit-pure
			)
			(cond		((null simple)		nil)	;no solution
						(t 					(let*	(
													(var 			(append (car simple) path))	;update assigned variables
													(new-delta		(second simple))			;update new delta
													)
													(cond	((null new-delta)		(complete-model n var))
															(t		(let*	(
																			(literal 	(most-used (shortest-list2 new-delta) new-delta));IMPORTANT	
																			;most-constrained variable first by using finding shortest-list from delta
																			;then most-constraining variable next by using most-used function
																			;THUS if list is shortest, each literal is most constrained by each other
																			;and among them, most used variable thru the entire delta will be most constraining
																			
																			;from here, below lines are [DPLL(delta and literal) or DPLL(delta and not(literal)]
																			(delta1 	(cons (list literal) new-delta))
																			(delta2 	(cons (list (* -1 literal)) new-delta))
																			(result1 	(sat1? n delta1 var))
																			)
																			(cond	((null result1)		(sat1? n delta2 var))
																					(t							result1)
																			)															
																	)															
															)
													
													)						
											)																
						)
			
			)	
	)
)

(defun sat? (n delta)	;just use above helper function with starting condition of path being nil
	(sat1? n delta nil)
)


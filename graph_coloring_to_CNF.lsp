;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "graph_coloring_to_CNF.lsp")
  );end defun

; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).

;;	variable index = (n-1)*k + c
(defun node2var (n c k)
	(+ c (* k (- n 1)))
  )

; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."

;;	it is supposed to return set of constraints variable with using (c | c+1 | ... | k) which also can be written as (c c+1 ... k)
;;	which means in place of c, c+1, ... and k from (c c+1 ... k), we replace them with node2var(n c K)
(defun at-least-one-color (n c k)
	(cond 	((> c k)	nil)
			(t	(cons (node2var n c k) (at-least-one-color n (+ c 1) k)))	
	)
  )

;;	helper function of at-most-one-color
;;	returns combinations of any possible two negated integers with fixed lower bound c1 and upper bound k
;;	for example, if c=1, k=3, then it returns (-1,-3),(-1,-2)
;;	for example, if c=2, k=5, then it returns (-2,-5),(-2,-4),(-2,-3)
(defun help1 (n c1 c2 k)
	(cond	((= c1 c2)	nil)
			(t		(cons (cons (* -1 (node2var n c1 k)) (list (* -1 (node2var n c2 k)))) (help1 n c1 (- c2 1) k)))
	)
)

; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."

;;	it returns any possible combination of two nagated integers starting from c to k
;;	it uses help1 as helper function
;;	for example, if c=1, k=3, then it returns (-1,-3),(-1,-2),(-2,-3)
;;	for example, if c=2, k=5, then it returns (-2,-5),(-2,-4),(-2,-3),(-3,-5),(-3,-4),(-4,-5)
(defun at-most-one-color (n c k)
	(cond 	((= c k)	nil)
			(t	(append (help1 n c k k) (at-most-one-color n (+ c 1) k)))	
	)
  )

; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."

;;	exactly one can be achieved by using at-least-one-color and at the same time at-most-one-color
(defun generate-node-clauses (n k)
	(cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )


;;	helper function of generate-edge-clauses
;;	this ensures two nodes n1 and n2 having different color from each other
;;	if two nodes have same color, then nth elements of each at-least-one-color set will have True and True. And if so, it should be false
;;	so I negated all elements from at-least-one-color and put them in disjunction
;;	then it returns false only when two elements at the same position are True and True. Otherwise, it returns True.
(defun help2 (n1 n2 c k)
	(cond	((> c k)	nil)
			(t 		(cons (cons (* -1 (first (at-least-one-color n1 c k))) (list (* -1 (first (at-least-one-color n2 c k))))) (help2 n1 n2 (+ c 1) k)))
	)
)

; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."

;;	basically it preserve constraints of each node from edge
;;	thus, it needs to append generate-node-clauses for each nodes together
;; 	and then, also append it with helper function we defined above to prohibit nodes x and y from having the same color
(defun generate-edge-clauses (e k)
	(append (generate-node-clauses (first e) k) (generate-node-clauses (second e) k) (help2 (first e) (second e) 1 k))
 )


; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun

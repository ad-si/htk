;;; -*- Package: INKA; Syntax: Common-lisp -*-

(in-package :INKA)

(DEFMACRO NEQ (S1 S2) `(NOT (EQ ,S1 ,S2)))

(defun prefix-p (seq1 seq2 &key (test #'eql))

  ;;; Edited  : 08. Jun 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (funcall test seq1 seq2)
      t
    (let ((result (search seq1 seq2 :test test)))
      (if (and result (eq 0 result)) (subseq seq2 (length seq1)) nil))))

(defun postfix-p (seq1 seq2 &key (test #'eql))

  ;;; Edited  : 01. Mar 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (funcall test (subseq seq2 (- (length seq2) (length seq1))) seq1)
      (subseq seq2 0 (- (length seq2) (length seq1)))
    nil))



(defmacro prefix-case (string &rest clauses)
  (let ((string-var (gensym)))
    `(let ((,string-var ,string))
       (cond ,@(mapcar #'(lambda (clause)
			   (prefix-case-massage-clause string-var clause))
		       clauses)
	     (t (error "~S fell through prefix-case." ,string-var))))))


(defun prefix-case-massage-clause (string-var clause)
  (let ((prefixes (etypecase (first clause)
		    (string (list (first clause)))
		    (list   (first clause))))
	(body     (rest clause)))
    `((some #'(lambda (x) (prefix-p x ,string-var)) 
	    ',prefixes)
      ,@body)))



;;; sdbm

;;; this algorithm was created for the sdbm (a reimplementation of ndbm) database library and seems to
;;; work relatively well in scrambling bits. the actual function is in the form hash(i) = hash(i - 1) *
;;; 65599 + str[i]; what is included below is the faster version used in gawk. the magic constant 65599
;;; was picked out of thin air, but turns out to be a prime.
;;; (Source: http://www.cs.yorku.ca/~oz/hash.html)

(defun sdbm-hash-value (str-list)

  ;;; Edited  : 22. May 2001
  ;;; Authors : serge       
  ;;; Input   : a string or a list of strings
  ;;; Effect  : /
  ;;; Value   : a hash value for the (concatenated) string

  (let ((hash-value 0))
    (dolist (str (if (stringp str-list) (list str-list) str-list))
      (loop for i from 0 to (1- (length str)) do
	(setq hash-value (+ (* hash-value 65599) (char-code (aref str i)))))
      )
    hash-value))


(defmacro find-duplicate (alist &key (test #'eq))

  ;;; Edited  : 05. Sep 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  `(let ((accumulator nil)
	 (result      nil))
     (dolist (elem ,alist)
       (if (member elem accumulator :test ,test)
	   (setq result elem)
	 (push elem accumulator)))
     result))


(defmacro smapcar (funct stepfunction &rest lists) 

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on the first elements of each list
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  the list of results of applying \verb$FUNCT$ to the elements of \verb$LISTS$
  
  (let (header local.vars end.test car.list)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq header (nconc header (list (list 'result nil))))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    (setq car.list (mapcar #'(lambda (var) `(car ,var)) local.vars))
    `(do ,header (,end.test result) (setq result (nconc result (list (funcall ,funct ,@car.list)))))))


(defmacro smapc (funct stepfunction &rest lists)

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on the first elements of each list
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  undefined
  
  (let (header local.vars end.test car.list)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    (setq car.list (mapcar #'(lambda (var) `(car ,var)) local.vars))
    `(do ,header (,end.test nil) (funcall ,funct ,@car.list))))


(defmacro smapcan (funct stepfunction &rest lists)

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on the first elements of each list
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  the concatenated results of applying \verb$FUNCT$ to the elements of \verb$LISTS$
  
  (let (header local.vars end.test car.list)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq header (nconc header (list (list 'result nil))))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    (setq car.list (mapcar #'(lambda (var) `(car ,var)) local.vars))
    `(do ,header (,end.test result) (setq result (nconc result (funcall ,funct ,@car.list))))))


(defmacro smaplist (funct stepfunction &rest lists)

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on \verb$LISTS$
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  the list of the results of applying \verb$FUNCT$ to the elements of \verb$LISTS$
  
  (let (header local.vars end.test)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq header (nconc header (list (list 'result nil))))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    `(do ,header (,end.test result) (setq result (nconc result (list (funcall ,funct ,@local.vars)))))))


(defmacro smapl (funct stepfunction &rest lists)

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on \verb$LISTS$
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  undefined
  
  (let (header local.vars end.test)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    `(do ,header (,end.test nil) (funcall ,funct ,@local.vars))))


(defmacro smapcon (funct stepfunction &rest lists)

  ;;; Input:  two functional and some lists
  ;;; Effect: applies \verb$FUNCT$ on \verb$LISTS$
  ;;;         and iterates this procedure to the lists obtained by applying \verb$STEPFUNCTION$
  ;;;         to each of the lists.
  ;;; Value:  the concatenated results of applying \verb$FUNCT$ to the \verb$LISTS$
  
  (let (header local.vars end.test)
    (setq local.vars (mapcar #'(lambda (list) (declare (ignore list)) (gensym "intern-")) lists))
    (setq header (mapcar #'(lambda (var list)
			     `(,var ,list (funcall ,stepfunction ,var)))
			 local.vars lists))
    (setq header (nconc header (list (list 'result nil))))
    (setq end.test (cons 'or (mapcar #'(lambda (var) `(endp ,var)) local.vars)))
    `(do ,header (,end.test result) (setq result (nconc result (funcall ,funct ,@local.vars))))))


(defmacro maxima (list &optional (predicate '(quote >)))

  ;;; Input:  a list and an ordering
  ;;; Value:  list of all maximal elements of \verb$LIST$ wrt \verb$PREDICATE$
  
  `(remove-if #'(lambda (elem)
		  (find-if #'(lambda (elem2)
			       (and (not (eq elem elem2))
				    (not (funcall ,predicate elem elem2))))
			   ,list))
	      ,list))


(defmacro addlast (elem list)

  ;;; Edited  : 20. Jul 2001
  ;;; Authors : serge       
  ;;; Input   : inserts the elem at the end of list
  ;;; Effect  : destructive, if the list is non-empty
  ;;; Value   : the (changed) list
  
  `(if (null ,list) (cons ,elem nil)
     (progn 
       (rplacd (last ,list) (cons ,elem nil))
       ,list)))


#-ALLEGRO-V6.0(DEFMACRO WHILE (CONDITION &Body BODY)

  ;;; Input:  an sexpression and a list of sexpression
  ;;; Effect: if \verb$ CONDITION$ evaluates to a non-NIL value \verb$BODY$ is executed.
  ;;;         This procedure is repeated until the execution of \verb$ CONDITION$ holds NIL.
  ;;; Value:  undefined
  
  (let ((LABEL (GENSYM "WHILE.TAG.")))
    `(tagbody ,LABEL
	      (when ,CONDITION
		,@BODY
		(GO ,LABEL)))))


#-ALLEGRO-V6.0(DEFMACRO UNTIL (CONDITION &body BODY)

  ;;; Input:  an sexpression and a list of sexpression
  ;;; Effect: \verb$BODY$ is executed until \verb$ CONDITION$ evaluates to NIL.
  ;;; Value:  undefined
  
  (let ((LABEL (GENSYM "UNTIL.TAG.")))
    `(tagbody ,LABEL
	      ,@ BODY
	      (unless ,CONDITION (GO ,LABEL)))))


(DEFMACRO CASSOC (ITEM A-LIST &REST KEYS)

  ;;; Input:  an atom, an assoc-list and a key-list for \verb$ASSOC$
  ;;; Value:  the cdr of the assoc-value of \verb$ITEM$ applied to \verb$A-LIST$
  
  (LIST 'CDR (CONS 'ASSOC (CONS ITEM (CONS A-LIST KEYS)))))


(defun compose-assoclists (al1 al2 &key (test #'eq))

  ;;; Edited  : 29. May 2002
  ;;; Authors : serge       
  ;;; Input   : two assoc-lists 
  ;;; Effect  : computes the composition assoc-list AL1 o AL2
  ;;; Value   : the new assoc list

  (append AL1 
	  (mapcar #'(lambda (key.entry)
		      (let ((temp (cassoc (cdr key.entry) al1 :test test)))
			(if temp (cons (car key.entry) temp) key.entry)))
		  al2)))


(DEFMACRO NCONC1 (LISTE ITEM)

  ;;; Input:  a list and a item
  ;;; Value:  \verb$ITEM$ is destructivly appended to the the end of \verb$LISTE$.
  
  `(NCONC ,LISTE (LIST ,ITEM)))


(DEFMACRO MAKE-EMPTY-STACK (PLACE)
  
  `(SETF ,PLACE NIL))


(DEFMACRO EMPTY-STACK-P (PLACE)
  `(NULL ,PLACE))


(DEFMACRO TOP (PLACE)
  `(CAR ,PLACE))


(DEFUN SUBPAIR (NEW OLD TREE &KEY SHARE (TEST (FUNCTION EQL))) 
  (NSUBPAIR NEW OLD (COPY-TREE TREE) :SHARE SHARE :TEST TEST))


(DEFUN NSUBPAIR (NEW OLD TREE &KEY SHARE (TEST (FUNCTION EQL)))  
  (when (and (not (SOME (FUNCTION
			  (LAMBDA (N O)
			    (COND ((FUNCALL TEST O TREE)
				   (SETQ TREE (IF (not SHARE)
						  (COPY-TREE N)
						  N))
				   T))))
			NEW OLD))
	     (consp tree))
    (RPLACA TREE (NSUBPAIR NEW OLD (CAR TREE) :SHARE SHARE :TEST TEST))
    (RPLACD TREE (NSUBPAIR NEW OLD (CDR TREE) :SHARE SHARE :TEST TEST)))
  TREE)


(DEFUN CONSES  (X)
  (LET ((N 0))
       (WHILE (CONSP X)
	      (SETQ N (+ (CONSES (CAR X))
			 1 N))
	      (SETQ X (CDR X)))
       N))


(DEFUN INSIDE (ITEM TREE &KEY (TEST (FUNCTION EQL)))

  ;;; Input:  an arbitrary lisp-object, a list and a testfunction
  ;;; Effect: the testfunction is applied to the object and successively to all 
  ;;;         subtrees of tree until the first application succeeds.
  ;;; Value:  then t is returned else if no application succeeds nil is returned.

  (COND ((FUNCALL TEST ITEM TREE) T)
	((CONSP TREE)
	 (OR (INSIDE ITEM (CAR TREE) :TEST TEST)
	     (INSIDE ITEM (CDR TREE) :TEST TEST)))
	(T NIL)))


(DEFUN FLATTEN (X)

  ;;; Input:  an sexpression
  ;;; Value:  a list, where each toplevel element is an atom occuring in x, 
  ;;;         multiple occurences in x result in multiple membership in (flatten x)

    (COND ((CONSP X)
           (MAPCAN (FUNCTION FLATTEN) X))
          ((NULL X) NIL) 
          (T (LIST X))))




(DEFUN FORMAT.ITERATE (CONTROL.STRING LIST)
  
  ;;; Input:   a string for the \verb$FORMAT$-command and a list
  ;;; Effect:  applies \verb$FORMAT$ to each element of \verb$LIST$ wrt. \verb$CONTROL.STRING$.
  ;;;          the results are concatenated by commas resp. \verb$and$ / \verb$or$.
  ;;; Value:   the generated string
  
  (COND ((NULL LIST) "")
	((NULL (CDR LIST)) (FORMAT NIL CONTROL.STRING (CAR LIST)))
	((NULL (CDDR LIST)) (FORMAT NIL "~A and ~A" (FORMAT NIL CONTROL.STRING (CAR LIST))
				    (FORMAT NIL CONTROL.STRING (SECOND LIST))))
	(T (FORMAT NIL "~A, ~A"
		   (FORMAT NIL CONTROL.STRING (CAR LIST))
		   (FORMAT.ITERATE CONTROL.STRING (CDR LIST))))))


(DEFMACRO ONLY-ONE (APPLY.FUNCTION LIST)

  ;;; Input :  an apply-function and a list
  ;;; Effect:  applies \verb$APPLY.FUNCTION$ on each element of \verb$LIST$ until \verb$APPLY.FUNCTION$ returns 
  ;;;          a sexpression =/ NIL for a second time.
  ;;; Value:   the only element, for which \verb$APPLY.FUNCTION$ returns a sexpression NIL, iff it is unique;
  ;;;          else NIL.

  (LET ((RESULT (GENSYM)) (ONLY.ONE (GENSYM)) (ELEMENT (GENSYM)))
    `(LET (,RESULT ,ONLY.ONE ,ELEMENT)
       (COND ((FIND-IF #'(LAMBDA (ELEM)
			   (COND ((SETQ ,RESULT (FUNCALL ,APPLY.FUNCTION ELEM))
				  (COND (,ONLY.ONE T)
					(T (SETQ ,ONLY.ONE T)
					   (SETQ ,ELEMENT ,RESULT)
					   NIL)))))
		       ,LIST)
	      NIL)
	     (T ,ELEMENT)))))



(DEFUN MAP.FLATTEN (BODY ARGLIST)

  ;;; Input : an apply-function and a list
  ;;; Effect: applies \verb$BODY$ on each element of \verb$ARGLIST$.
  ;;; Value:  the modified \verb$ARGLIST$ in which each element of \verb$ARGLIST$, \verb$BODY$ results in an sexpression s
  ;;;         (which has to be a list), is replaced by the list s on top-level.

  (LET (FIRST POINTER RESULT ELEMS)
    (COND (ARGLIST (SETQ FIRST (FUNCALL BODY (CAR ARGLIST)))
		   (SETQ ELEMS ARGLIST)
		   (WHILE (CDR ELEMS)
		     (COND ((SETQ RESULT (FUNCALL BODY (SECOND ELEMS)))
			    (SETQ POINTER (LAST RESULT))
			    (SETF (CDR POINTER) (CDDR ELEMS))
			    (SETF (CDR ELEMS) RESULT)
			    (SETQ ELEMS POINTER))
			   (T (POP ELEMS))))
		   (COND (FIRST (NCONC FIRST (CDR ARGLIST)))
			 (T ARGLIST))))))


(DEFUN LASTN (LIST N)

  ;;; Input:  a list and a natural number
  ;;; Value:  the last \verb$N$-th elements of \verb$LIST$.
  
  (cond ((>= (length list) n)
	 (NTHCDR (- (LENGTH LIST) N) LIST))))


(DEFMACRO SOMEF (BODY PROP.LIST)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$ until
  ;;;         some call returns a non-NIL value
  ;;; Value:  the result of the last execution of \verb$BODY$

  (LET ((LOCAL.VAR  (gensym "intern-")))
       `(LET (,LOCAL.VAR)
	     (DO* ((TAIL ,PROP.LIST (CDDR TAIL))
		   (INDICATOR (CAR TAIL) (CAR TAIL))
		   (VALUE (SECOND TAIL) (SECOND TAIL)))
		  ((OR ,LOCAL.VAR (NULL TAIL)) ,LOCAL.VAR)
		  (SETQ ,LOCAL.VAR (FUNCALL ,BODY INDICATOR VALUE))))))


(DEFMACRO EVERYF (BODY PROP.LIST)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$ until
  ;;;         some call returns NIL.
  ;;; Value:  the result of the last execution of \verb$BODY$
  
  (LET ((LOCAL.VAR  (gensym "intern-")))
       `(LET ((,LOCAL.VAR T))
	     (DO* ((TAIL ,PROP.LIST (CDDR TAIL))
		   (INDICATOR (CAR TAIL) (CAR TAIL))
		   (VALUE (SECOND TAIL) (SECOND TAIL)))
		  ((OR (NULL ,LOCAL.VAR) (NULL TAIL)) ,LOCAL.VAR)
		  (SETQ ,LOCAL.VAR (FUNCALL ,BODY INDICATOR VALUE))))))


(defmacro mapcarf (body prop.list)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$ until
  ;;;         some call returns a non-NIL value
  ;;; Value:  the result of the last execution of \verb$BODY$

  (let ((local.var  (gensym "intern-")))
       `(let (,local.var)
	     (do* ((tail ,prop.list (cddr tail))
		   (indicator (car tail) (car tail))
		   (value (second tail) (second tail)))
		  ((null tail) (reverse ,local.var))
	       (setq ,local.var (cons (funcall ,body indicator value) ,local.var))))))

(defmacro mapcanf (body prop.list)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$ until
  ;;;         some call returns a non-NIL value
  ;;; Value:  the result of the last execution of \verb$BODY$

  (let ((local.var  (gensym "intern-")))
       `(let (,local.var)
	     (do* ((tail ,prop.list (cddr tail))
		   (indicator (car tail) (car tail))
		   (value (second tail) (second tail)))
		 ((null tail) ,local.var)
	       (setq ,local.var (append ,local.var (funcall ,body indicator value)))))))


(DEFMACRO MAPCF (BODY PROP.LIST)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$
  ;;; Value:  the result of the last execution of \verb$BODY$
  
  (LET ((LOCAL.VAR  (gensym "intern-")))
       `(LET ((,LOCAL.VAR T))
	     (DO* ((TAIL ,PROP.LIST (CDDR TAIL))
		   (INDICATOR (CAR TAIL) (CAR TAIL))
		   (VALUE (SECOND TAIL) (SECOND TAIL)))
		  ((NULL TAIL) ,LOCAL.VAR)
		  (SETQ ,LOCAL.VAR (FUNCALL ,BODY INDICATOR VALUE))))))


(DEFMACRO SUBSETF (BODY PROP.LIST)

  ;;; Input:  a functional with two arguments and a propertylist
  ;;; Effect: calls \verb$BODY$ with each pair indicator and value of \verb$PROP.LIST$ and
  ;;;         collects all pairs for which \verb$BODY$ results in a non-NIL value.
  ;;; Value:  the new property list
  
  (LET ((LOCAL.VALUE  (gensym "intern-")))
       `(LET (,LOCAL.VALUE)
	     (DO* ((TAIL ,PROP.LIST (CDDR TAIL))
		   (INDICATOR (CAR TAIL) (CAR TAIL))
		   (VALUE (SECOND TAIL) (SECOND TAIL)))
		  ((NULL TAIL) ,LOCAL.VALUE)
		  (COND ((FUNCALL ,BODY INDICATOR VALUE)
			 (SETQ ,LOCAL.VALUE (NCONC ,LOCAL.VALUE
						   (LIST INDICATOR VALUE)))))))))
  

(defmacro find-all (pred seq)

  ;;; Input:  a functional \verb$PRED$ and a sequence \verb$SEQ$
  ;;; Effect: \verb$PRED$ is applied to all elements of \verb$SEQ$
  ;;; Value:  a list of all elements of \verb$SEQ$ which satisfy the test specified by \verb$PRED$

  (let ((new-symbol (gensym "intern-")))
    `(mapcan #'(lambda (,new-symbol) (cond ((funcall ,pred ,new-symbol) (list ,new-symbol))))
	     ,seq)))


(defun powerset (set &key (test 'equal))

  ;;; Input:  a list of objects and a function testing whether two sets are equal.
  ;;; Effect: computes the powerset of set where all duplicates (modulo test.fct)
  ;;;         are removed.
  ;;; Value:  the generated powerset

  (delete-duplicates (powerset.1 set) :test test))


(defun powerset.1 (set)
  
  (cond ((null set) (list nil))
	(t (let ((rest.powerset (powerset.1 (cdr set))))
	     (append rest.powerset (mapcan #'(lambda (sub.set)
					       (list (cons (car set) sub.set)))
					   rest.powerset))))))


(defun set.all.n-tuples (set n)

  ;;; Edited  : 05. Jul 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (assert (>= n 0))
  (if (= n 0) nil
    (if (= n 1) (mapcar #'list set)
      (let ((n-1-tuples (set.all.n-tuples set (1- n))))
	(mapcan #'(lambda (elem)
		    (mapcar #'(lambda (tuple) (cons elem tuple)) n-1-tuples))
		set)))))


(defun copy-file (from.file to.file &key (if-exists :append) (characters t))
  (declare (ignore characters))
  (if (not (probe-file to.file))
      (rename-file from.file to.file)
      (let ((to.stream (open to.file :if-exists if-exists
			             :if-does-not-exist :create
				     :direction :output))
	    (from.stream (open from.file :if-does-not-exist :error
			                 :direction :input))
	    (line nil))
	(do ()
	    ((eq line :eof))
	  (setf line (read-line from.stream nil :eof))
	  (unless (eq line :eof)
	    (write-line line to.stream)))
	(close to.stream)
	(close from.stream)
	t
	)))

(defun command-line-arguments ()

  ;;; Edited  : 30. May 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+ALLEGRO (cdr (sys::command-line-arguments))
  #+CMU18   (cdddr ext:*command-line-strings*)
  )


(provide "service")



(in-package :cl-user)

(defun xml-entity.create (name attributes content)

  ;;; Edited  : 05. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (make-xml*entity :name name :attribute attributes :content content))


(defun xml-attribute.create (name value)

  ;;; Edited  : 05. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (make-xml*attribute :name name :content value))


(defun xml-make.xmldecl (&optional (xml-version 1.0) 
				   (encoding "utf-8"))

  ;;; Edited  : 05. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (assert (numberp xml-version))
  (format nil "<?xml version=\"~D\" encoding=\"~A\"?>" xml-version encoding))


(defun xml-make.doctypedecl (name dtd-file)

  ;;; Edited  : 05. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (format nil "<!DOCTYPE ~A SYSTEM \"~A\"!>" name dtd-file))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defstruct (xml*entity (:conc-name xml-entity.) (:print-function xml=entity.print))
  name
  attribute
  content)

(defun xml=entity.print (entity stream depth)

  ;;; Edited  : 21. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (format stream "<~A ~{~A~^ ~}" (xml-entity.name entity) (xml-entity.attribute entity))
  (if (xml-entity.content entity) 
      (format stream "> ... </~A>" (xml-entity.name entity))
    (format stream "/>")))


(defun xml-entity.print (entity stream &optional (depth 0))

  ;;; Edited  : 10. Dec 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (typep entity 'xml*entity)
      (progn 
	(loop for j from 1 to depth do (format stream " "))
	(format stream "<~A~{ ~A~^~}" 
		(xml-entity.name entity) 
		(xml-entity.attribute entity))
	(if (xml-entity.content entity) 
	    (progn (format stream ">~%")
		   (mapc #'(lambda (subentity)
			     (xml-entity.print subentity stream (1+ depth)))
			 (xml-entity.content entity))
		   (loop for j from 1 to depth do (format stream " "))
		   (format stream "</~A>~%" (xml-entity.name entity)))
	  (format stream "/>~%")))
    (progn (loop for j from 1 to depth do (format stream " "))
	   (write-string entity stream)
	   (write-char #\newline stream))))


;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defstruct (xml*attribute (:conc-name xml-attribute.) (:print-function xml=attribute.print))
  name
  content)

(defun xml=attribute.print (attribute stream depth)

  ;;; Edited  : 21. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (format stream "~A=~S" (xml-attribute.name attribute) (xml-attribute.content attribute)))

;;;Edited   : 11.2002
;;;Authors  : shaoning
;;;Input    : XML text as a string
;;;Effect   : Interface for xml-parser.lisp
;;;Value    : a defined deta structrue


(defun xml-map.entity (entity closure &key top-p results)

  ;;; Edited  : 30. May 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (typep entity 'xml*entity)
      (let ((closurevalue (funcall closure entity)))
	(if closurevalue
	    (let ((newresults (cons closurevalue results)))
	      (unless top-p
		(dolist (subentity (xml-entity.content entity))
		  (setq newresults (xml-map.entity subentity closure 
						   :top-p top-p 
						   :results newresults))))
	      newresults)
	  (progn 
	    (dolist (subentity (xml-entity.content entity))
	      (setq results (xml-map.entity subentity closure :top-p top-p 
					    :results results)))
	    results)))
    results))



;;;------------------------------------------------------
;;;
;;; Chapter 1 : 
;;;
;;;------------------------------------------------------
    
    
(defun xml-entity.are.equal (ent1 ent2)

   ;;; Edited  : 17. Jan 2002 05. Mar 2004
   ;;; Authors : serge        serge       
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

  (cond ((and (stringp ent1) (stringp ent2))
	 (string-equal ent1 ent2))
	((and (xml*entity-p ent1) (xml*entity-p ent2))
	 (and (string-equal (xml-entity.name ent1) (xml-entity.name ent2))
	      (subsetp (xml-entity.attribute ent1) (xml-entity.attribute ent2) 
		       :test #'xml-attribute.are.equal)
	      (= (length (xml-entity.content ent1)) (length (xml-entity.content ent2)))
	      (every #'xml-entity.are.equal 
		     (xml-entity.content ent1) 
		     (xml-entity.content ent2))))
	))


(defun xml-attribute.are.equal (attr1 attr2)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (and (string-equal (xml-attribute.name attr1) (xml-attribute.name attr2))
        (string-equal (xml-attribute.content attr1) (xml-attribute.content attr2))))


(defmacro xml-entity.case (entity &rest clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (let ((entity-var (gensym)))
     `(let ((,entity-var ,entity))
        ,(xml=generate.entity.case.code entity-var clauses))))


;; Beispiel:

(defmacro xml-attribute.case (attribute &rest clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (let ((attribute-var (gensym)))
     `(let ((,attribute-var ,attribute))
        ,(xml=generate.attribute.case.code attribute-var clauses))))


(defun xml=generate.entity.case.code (entity-var clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (if (endp clauses)
       `(error "Entity fell through an ENTITY-CASE form:~&`'")
     (let* ((pattern   (first (first clauses)))
           (body      (rest (first clauses)))
           (free-vars (xml-entity.pattern.free.vars pattern))
           (alist-var (gensym)))
       `(let ((,alist-var (xml=entity.match ',pattern ,entity-var)))
         (if (eq ,alist-var t)
             ,(xml=generate.entity.case.code entity-var (rest clauses))
           (let ,(mapcar #'(lambda (var)
                             `(,var (cdr (assoc ',var ,alist-var))))
                         free-vars)
             ,@body))))))


(defun xml=generate.attribute.case.code (attribute-var clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (if (endp clauses)
       `(error "Attribute fell through an ATTRIBUTE-CASE form:~&`~A'" 
,attribute-var)
     (let* ((pattern   (first (first clauses)))
           (body      (rest (first clauses)))
           (free-vars (xml-attribute.pattern.free.vars pattern))
           (alist-var (gensym)))
       `(let ((,alist-var (xml=attribute.match ',pattern ,attribute-var)))
         (if (eq ,alist-var t)
             ,(xml=generate.attribute.case.code attribute-var (rest clauses))
           (let ,(mapcar #'(lambda (var)
                             `(,var (cdr (assoc ',var ,alist-var))))
                         free-vars)
             ,@body))))))


(defun xml=entity.match (pattern entity &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (if (eq bindings t) t
     (case (xml=entity.pattern.type pattern)
       (:variable
        (let ((bind (xml=lookup-binding pattern bindings)))
         (cond ((not bind)
                (xml=add-binding pattern entity bindings))
               ((xml-entity.are.equal (cdr bind) entity)
                bindings)
               (t t))))
       (:literal
        (if (and (or (stringp entity) (symbolp entity))
                (string-equal pattern entity))
           bindings
         t)
        )
       (:application
        (if (xml*entity-p entity)
           (if (string-equal (first pattern) (xml-entity.name entity))
               (let ((bind (xml=attribute.match* (second pattern) (xml-entity.attribute 
entity) bindings)))
                 (if (not (eq bind t))
                     (xml=entity.match* (third pattern) (xml-entity.content entity) bind)
                   t))
             t)
         t))
       (t (error "Unknown pattern type `~S' (this is a bug)" pattern)))))


(defun xml=entity.match* (pattern entity-list &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=entity.pattern.type pattern)
     (:empty
      (if (endp entity-list) bindings
        t))
     (:variable
      (let ((bind (xml=lookup-binding pattern bindings)))
        (if bind t
         (xml=add-binding pattern entity-list bindings))))
     (:list
      (let ((bind (xml=entity.match (first pattern) (first entity-list) bindings)))
        (if (not (eq bind t))
           (xml=entity.match* (rest pattern) (rest entity-list) bind)
         t)))))


(defun xml=attribute.match (pattern attribute &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=attribute.pattern.type pattern)
     (:variable
      (let ((bind (xml=lookup-binding pattern bindings)))
        (cond ((not bind)
              (xml=add-binding pattern attribute bindings))
             ((xml-attribute.are.equal (cdr bind) attribute)
              bindings)
             (t t))
        ))
     (:pattern
      (cond ((string-equal (first pattern) (xml-attribute.name attribute))
            (let ((bind (xml=lookup-binding (second pattern) bindings)))
              (cond ((not bind)
                     (xml=add-binding (second pattern) (xml-attribute.content attribute) bindings))
                    ((string-equal (cdr bind) (xml-attribute.content attribute))
                     bindings)
                    (t t))))
           (t t)))))

(defun xml=attribute.match* (pattern attr-set &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=attribute.set.pattern.type pattern)
     (:variable
      (let ((bind (xml=lookup-binding pattern bindings)))
        (cond ((not bind)
              (xml=add-binding pattern attr-set bindings))
             ((and (= (length (cdr bind)) (length attr-set))
                   (subsetp (cdr bind) attr-set :test #'xml-attribute.are.equal)
                   (subsetp attr-set (cdr bind) :test #'xml-attribute.are.equal))
              bindings)
             (t t))))
     (:empty bindings)
     (:list
      (let ((attr (find-if #'(lambda (attribute)
                              (not (eq t (xml=attribute.match (first pattern) attribute bindings))))
                          attr-set)))
        (cond (attr
              (xml=attribute.match* (rest pattern)
                                    (remove attr attr-set)
                                    (xml=attribute.match (first pattern) attr bindings)))
             (t t))))))


(defun xml=lookup-binding (var bindings)
   (assoc var bindings))

(defun xml=add-binding (key value bindings)
   (acons key value bindings))


(defun xml=entity.pattern.type (x)
   (cond ((stringp x)                            :literal)
        ((and (symbolp x)
              x)                                :variable)
        ((consp x)
         (cond ((and (= 3 (length x))
                     (stringp (first x))) :application)
               (t (error "Invalid pattern `~S'" x))))))

(defun xml=entity-list.pattern.type (x)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (cond ((and (symbolp x)
              x)                                :variable)
        ((endp x)                               :empty)
        ((consp x)                              :list)
        (t (error "Invalid entity-list pattern `~S'" x))))



(defun xml=attribute.pattern.type (x)
   (cond ((stringp x)                           (error "Invalid attribute 
pattern `~S'" x))
        ((and (symbolp x)
              x)                               :variable)
        ((consp x) (cond ((= (length x) 2)
                          (cond ((and (stringp (first x))
                                      (symbolp (second x))
                                      (second x))     :pattern)
                                (t                    (error "Invalid attribute-name pattern `~S'" (first x)))))
                         (t                    (error "Invalid (non-binary) attribute pattern `~S'." x))))))


(defun xml=attribute.set.pattern.type (x)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (cond ((stringp x) (error "Invalid attribute-set pattern `~S'" x))
        ((and (symbolp x)
              x)                  :variable)
        ((consp x)                :list)
        ((endp x)                 :empty)))



(defun xml-entity.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (remove-duplicates (xml=entity.pattern.free.vars pattern)))

(defun xml-attribute.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (remove-duplicates (xml=attribute.pattern.free.vars pattern)))


(defun xml=entity.pattern.free.vars (pattern)
   (case (xml=entity.pattern.type pattern)
     (:variable    (list pattern))
     (:literal     '())
     (:application (append
                   (xml=attribute.set.free.vars (second pattern))
                   (xml=entity.list.pattern.free.vars (third pattern))))))


(defun xml=entity.list.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=entity-list.pattern.type pattern)
     (:variable (list pattern))
     (:empty    '())
     (:list     (append (xml=entity.pattern.free.vars (car pattern))
                       (xml=entity.list.pattern.free.vars (cdr pattern))))))


(defun xml=attribute.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=attribute.pattern.type pattern)
     (:variable (list pattern))
     (:pattern  (list (second pattern)))))


(defun xml=attribute.set.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (xml=attribute.set.pattern.type pattern)
     (:variable (list pattern))
     (:empty    '())
     (:list     (append (xml=attribute.pattern.free.vars (car pattern))
                       (xml=attribute.set.free.vars (rest pattern))))))



#+OLD(defun xml-runParser (aString)
  (if (not (compiled-function-p 'xmldiff=string-convert2xml))
      (compile 'xmldiff=string-convert2xml))
  (let ((thisString (xmldiff=string-convert2xml aString)))
    (xml-xml2struct thisString)))


(defun xml-runParser (aString)
  (mapcar #'xml-netxmlparser2xmlparser (net.xml.parser::parse-xml astring)))


(defun xml-parse.file (filename)

  ;;; Edited  : 09. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((result (with-open-file (stream filename)
		  (net.xml.parser::parse-xml stream))))
    (mapcar #'xml-netxmlparser2xmlparser result)))


(defun xml-netxmlparser2xmlparser (netxmlentity)

  ;;; Edited  : 09. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (cond ((stringp netxmlentity) netxmlentity)
	((constantp netxmlentity) (string netxmlentity))
	((listp netxmlentity)
	 (let ((head (first netxmlentity))
	       (content (mapcar #'xml-netxmlparser2xmlparser (rest netxmlentity))))
	   (multiple-value-bind (elementname attributes)
	       (xml=netxmlparser-head-translate head)
	     (xml-entity.create elementname attributes content))))
	(t (error "Undefined case"))))

(defun xml=netxmlparser-head-translate (head)

  ;;; Edited  : 09. Mar 2004
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 
  
  (let*((symbol (if (symbolp head) head (car head)))
	(netxmlattributes (if (symbolp head) nil (rest head)))
	(elementname (if (constantp symbol) symbol (string symbol)))
	(attributes (if (constantp symbol) nil
		      (mapcarf #'(lambda (name value)
				   (xml-attribute.create name value))
			       netxmlattributes)))
	)
    (values elementname attributes)))



(defun xmldiff=string-convert2xml (arg &optional start stream)

  ;;; Edited  : 29. Oct 2002 15. Nov 2002
  ;;; Authors : serge        serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (and start stream)
      (let ((length-arg (length arg)))
	(loop while (<= start length-arg) do
	  (multiple-value-bind (prefix-endpos char reststring-startpos) 
	      (xmldiff=string-find-next-code arg start)
	    (cond (char 
		   (write-string arg stream :start start :end prefix-endpos)
		   (write-char char stream)
		   (setq start reststring-startpos)
		   )
		  (t (write-string arg stream :start start)
		     (setq start (1+ length-arg))
		     )))))
    (with-output-to-string 
      (stream)
      (xmldiff=string-convert2xml arg 0 stream))))


(defun xmldiff=string-find-next-code (arg start)

  ;;; Edited  : 29. Oct 2002 15. Nov 2002
  ;;; Authors : serge        serge       
  ;;; Input   : a string 
  ;;; Effect  : searches for the next pattern "&#n;" where `n' is an integer
  ;;; Value   : if it could be found, returns the prefix before this pattern, the (code-char n), 
  ;;;           and the rest string; if no pattern is found, returns the whole string

  (let ((and-pos (position #\& arg :start start)))
    (if (and and-pos 
	     (eq #\# (elt arg (1+ and-pos))))
	(let ((semicolon-pos (position #\; arg :start (1+ and-pos))))
	  (if semicolon-pos
	      (let ((code-char (parse-integer arg 
					      :start  (+ and-pos 2) 
					      :end semicolon-pos)))
		(if (integerp code-char)
		    (values and-pos 
			    (code-char code-char)
			    (1+ semicolon-pos)) 
		  nil))
	    nil))
      nil)))

(defun xml-xml2struct (xmlstring)

  ;;; Edited  : 30. May 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (xml=buildStruct (xml=tokenize xmlstring)))

;;;Edited   : 1.2002
;;;Authors  : shaoning
;;;Input    :
;;;Effect   : extracting elements as tokens from XML(string) 
;;;Value    : 
(defun xml=tokenize (aString)
  (let ((result nil)
	(mode? NIL)
	(prefix "")
	(update 1)
	(length (length aString)))
	(do ((start 0 update))
	    ((<= length update) (if (emptystring-p prefix)
				    result
				  (append result (list (string-trim " " prefix))))) 
	  (cond
	   ((eq (aref aString start) #\")
	    (let ((pos (position #\" aString :start (+ start 1))))
	      (progn
		(setf prefix (with-output-to-string (stream) (write-string aString stream :start start :end (+ pos 1))))
		(setf update (+ pos 1)))))
	   ((and (eq (aref aString start) #\/)
		 (eq (aref aString (+ start 1)) #\>))
	    (progn
	      (setf update (+ start 2))
	      (setf result (append (append result (list (string-trim " " prefix))) (list "/>")))
	      (setf prefix "")))
	   ((eq (aref aString start) #\>)
	    (progn 
	    (setf result (append result (list (concatenate 'string (string-trim " " prefix) ">"))))
	    (setf prefix "")
	    (setf update (+ start 1))))
	   ((eq (aref aString start) #\<)
	    (let ((pos (position #\> aString :start (+ start 1))))
	      (progn
		(setf result 
		      (if (emptystring-p prefix)
			  result
			(append result (list (string-trim " " prefix)))))
		(if (eq (aref aString (- pos 1)) #\/)
		    (progn
		      (setf prefix (with-output-to-string (stream) (write-string aString stream :start start :end (- pos 1))))
		      (setf update (- pos 1)))
		  (progn 
		    (setf prefix (with-output-to-string (stream) (write-string aString stream :start start :end (+ pos 1))))
		    (setf update (+ pos 1))
		    (setf result (append result (list (string-trim " " prefix))))
		    (setf prefix ""))))))
	   ((member (aref aString 0) '(#\tab #\newline #\return #\linefeed))
	    (setf update (+ start 1)))
	   (t 
	    (progn
	      (setf prefix (with-output-to-string (stream) 
						  (write-string (string prefix) stream) 
						  (write-string aString stream :start start :end (+ start 1))))
	      (setf update (+ start 1))))))))



(defun emptystring-p (string)

   ;;; Edited  : 14. Aug 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (cond ((string-equal string "") t)
        ((eq #\space (aref string 0))
         (emptystring-p (subseq string 1)))))





;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=attr2list (aString)
  (if (equal aString nil)
      nil
    (xml=attrTokenize aString "" nil)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=attrTokenize (aString prefix result)
  (cond
   ((string-equal aString "")
    (progn
      (assert (listp result))
      (if (equal prefix "") 
	  (reverse result)
	(reverse (cons prefix result)))))
   ((eq (aref aString 0) #\")
    (let* ((posD (position #\" aString))
	   (posDD (position #\" aString :start (+ posD 1))))
      (xml=attrTokenize (subseq aString (+ posDD 1)) (concatenate 'string prefix (subseq aString 0 (+ posDD 1))) result)))
   ((eq (aref aString 0) #\=)
    (let ((posS (position #\space (string-left-trim " " prefix) :from-end T)))
      (if posS
	  (xml=attrTokenize (concatenate 'string (subseq prefix (+ posS 1)) aString) "" (cons (subseq prefix 0 posS) result))
	(xml=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))
   ((eq (aref aString 0) #\space)
    (if (equal prefix "")
	(xml=attrTokenize (subseq aString 1) "" result)
      (progn 
	(assert (listp result))
	(xml=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))
   (t (xml=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=buildAttr (aList)
  (if (equal aList nil)
      nil
    (cons (make-xml*attribute :name 
			      (subseq (car aList) 
				      0 (position #\= (car aList)))
			      :content 
			      ;(xml=getThemOut (xml=ascII2char (subseq (car aList) 
								     ;(+ 1 (position #\= (car aList)))))))
			      (xml=getThemOut (subseq (car aList)
						      (+ 1 (position #\= (car aList))))))
	  (xml=buildAttr (cdr aList)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=buildStruct (aList)
  (let (value pos)
    (cond
     ((equal aList nil)
      nil)
     ((equal (length aList) 1)
      ;(list (xml=getThemOut (xml=ascII2char (car aList)))))
      (list (xml=getThemOut (car aList))))
     ((xml=isComment? (xml=remove (car aList)))
      (xml=buildStruct (cdr aList)))
     (t
      (let* ((value (remove #\tab (remove #\space (xml=remove (xml=isEntity? (car aList))))))
	     (pos (xml=findTag aList value)))
	(if value
	    (if pos
		(if (equal pos (- (length aList) 1))
		    (list (make-xml*entity :name value
					   :attribute (xml=buildAttr (xml=attr2list (xml=getAttr (car aList))))
					   :content (xml=buildStruct (subseq aList 1 pos))))
		  (append (xml=buildStruct (subseq aList 0 (+ pos 1)))
			  (xml=buildStruct (subseq aList (+ pos 1)))))
	      (ERROR "No endtag for ~A in ~A" value aList))
	  (xml=buildStruct (cdr aList))))))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=isComment? (aString)
 (if (xml=isEntity? aString)
     (or
      (and 
       (equal (aref aString 1) #\?)
       t)
      ; (equal (aref aString (- (length aString) 2)) #\?))
      (equal (aref aString 1) #\!))
   nil))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
  
(defun xml=findTag (aList anEntity)
  (xml=findTagHelp (cdr aList) (xml=remove anEntity) 1 0 (car aList)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
   

(defun xml=findTagHelp (aList anEntity counter position last)
  (let ((map (xml=remove (car aList))))
    (cond
     ((eq counter 0) 
      position)
     ((equal aList nil)
      nil)
     ((xml=tagClose? map anEntity last)
      (xml=findTagHelp (cdr aList) anEntity (- counter 1) (+ position 1) map))
     ((xml=tagOpen? map anEntity)
      (xml=findTagHelp (cdr aList) anEntity (+ counter 1) (+ position 1) map))
     (t 
      (xml=findTagHelp (cdr aList) anEntity counter (+ position 1) map)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : there strings
;;;Effect   : check if the first one(object) is in form of "</anEntity>" or if object "/>" is and the same time (xml=tagOpen? last anEntity) T returns
;;;Value    : if yes T others nil

(defun xml=tagClose? (astring anEntity last)
  (let ((object (remove #\tab (remove #\Newline (remove #\Space astring)))))
    (or 
     (string-equal (concatenate 'string "</" (concatenate 'string anEntity ">")) 
		   object)
     (and (xml=tagOpen? last anEntity) 
	  (string-equal "/>" object)))))
  
(defun xml=remove (astring)
  (remove #\tab (remove #\Newline astring)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : two strings
;;;Effect   : check if the first one(object) is in one form of "<anEntity>" or "<anEntity"   
;;;Value    : if yes T others nil
   

(defun xml=tagOpen? (object anEntity)
  (if (or
       (string-equal (concatenate 'string "<" (concatenate 'string anEntity ">")) 
		     object)
       (and (<= (+ (length anEntity) 2) (length object))
	    (string-equal (concatenate 'string "<" (concatenate 'string anEntity " ")) 
			  (subseq object 0 (+ (length anEntity) 2)))))
      (progn ;; (warn "Identifying opentag for ~A for ~A" anentity object)
	     t)
    nil)
  )

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : a string
;;;Effect   : check if this string is in form "<...>"
;;;Value    : if yes T others nil

(defun xml=isEntity? (aString)
    (if  (< (length aString) 1)
	nil
      (if (and (equal (aref aString 0) #\<)
	       (not (equal (aref aString 1) #\/)))
	  (if (position #\space aString)
	      (subseq aString 1 (position #\space aString))
	    (if (equal (aref aString (- (length aString) 1)) #\>)
		(subseq aString 1 (- (length aString) 1))
	      (subseq aString 1)))
	  nil)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun xml=getAttr (aString)
  (let ((pos (position #\space aString)))
    (if (equal pos nil)
	nil
      (subseq aString (+ pos 1) (position #\> aString)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
  
(defun xml=getThemOut (aString)
  (string-trim "\"" aString))


  
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


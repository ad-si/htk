(in-package :dgrl)

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defstruct (omd*entity (:conc-name omd-entity.) (:print-function omd=entity.print))
  name
  attribute
  content)

(defun omd=entity.print (entity stream depth)

  ;;; Edited  : 21. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (format stream "<~A ~{~A~^ ~}" (omd-entity.name entity) (omd-entity.attribute entity))
  (if (omd-entity.content entity) 
      (format stream "> ... </~A>" (omd-entity.name entity))
    (format stream "/>")))


;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defstruct (omd*attribute (:conc-name omd-attribute.) (:print-function omd=attribute.print))
  name
  content)

(defun omd=attribute.print (attribute stream depth)

  ;;; Edited  : 21. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (format stream "~A=~S" (omd-attribute.name attribute) (omd-attribute.content attribute)))

;;;Edited   : 11.2002
;;;Authors  : shaoning
;;;Input    : OMDOC text as a string
;;;Effect   : Interface for omd-parser.lisp
;;;Value    : a defined deta structrue


(defun omd-map.entity (entity closure &key top-p results)

  ;;; Edited  : 30. May 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (typep entity 'omd*entity)
      (let ((closurevalue (funcall closure entity)))
	(if closurevalue
	    (let ((newresults (cons closurevalue results)))
	      (unless top-p
		(dolist (subentity (omd-entity.content entity))
		  (setq newresults (omd-map.entity subentity closure 
						   :top-p top-p 
						   :results newresults))))
	      newresults)
	  (progn 
	    (dolist (subentity (omd-entity.content entity))
	      (setq results (omd-map.entity subentity closure :top-p top-p 
					    :results results)))
	    results)))
    results))



;;;------------------------------------------------------
;;;
;;; Chapter 1 : 
;;;
;;;------------------------------------------------------
    
    
(defun omd-entity.are.equal (ent1 ent2)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (and (string-equal (omd-entity.name ent1) (omd-entity.name ent2))
        (subsetp (omd-entity.attribute ent1) (omd-entity.attribute ent2) :test 
#'omd-attribute.are.equal)
        (= (length (omd-entity.content ent1)) (length (omd-entity.content ent2)))
        (every #'omd-entity.are.equal (omd-entity.content ent1) 
(omd-entity.content ent2))))


(defun omd-attribute.are.equal (attr1 attr2)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (and (string-equal (omd-attribute.name attr1) (omd-attribute.name attr12))
        (string-equal (omd-attribute.content attr1) (omd-attribute.content 
attr12))))


(defmacro omd-entity.case (entity &rest clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (let ((entity-var (gensym)))
     `(let ((,entity-var ,entity))
        ,(omd=generate.entity.case.code entity-var clauses))))


;; Beispiel:

(defmacro omd-attribute.case (attribute &rest clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (let ((attribute-var (gensym)))
     `(let ((,attribute-var ,attribute))
        ,(omd=generate.attribute.case.code attribute-var clauses))))


(defun omd=generate.entity.case.code (entity-var clauses)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (if (endp clauses)
       `(error "Entity fell through an ENTITY-CASE form:~&`'")
     (let* ((pattern   (first (first clauses)))
           (body      (rest (first clauses)))
           (free-vars (omd-entity.pattern.free.vars pattern))
           (alist-var (gensym)))
       `(let ((,alist-var (omd=entity.match ',pattern ,entity-var)))
         (if (eq ,alist-var t)
             ,(omd=generate.entity.case.code entity-var (rest clauses))
           (let ,(mapcar #'(lambda (var)
                             `(,var (cdr (assoc ',var ,alist-var))))
                         free-vars)
             ,@body))))))


(defun omd=generate.attribute.case.code (attribute-var clauses)

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
           (free-vars (omd-attribute.pattern.free.vars pattern))
           (alist-var (gensym)))
       `(let ((,alist-var (omd=attribute.match ',pattern ,attribute-var)))
         (if (eq ,alist-var t)
             ,(omd=generate.attribute.case.code attribute-var (rest clauses))
           (let ,(mapcar #'(lambda (var)
                             `(,var (cdr (assoc ',var ,alist-var))))
                         free-vars)
             ,@body))))))


(defun omd=entity.match (pattern entity &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (if (eq bindings t) t
     (case (omd=entity.pattern.type pattern)
       (:variable
        (let ((bind (omd=lookup-binding pattern bindings)))
         (cond ((not bind)
                (omd=add-binding pattern entity bindings))
               ((omd-entity.are.equal (cdr bind) entity)
                bindings)
               (t t))))
       (:literal
        (if (and (or (stringp entity) (symbolp entity))
                (string-equal pattern entity))
           bindings
         t)
        )
       (:application
        (if (omd*entity-p entity)
           (if (string-equal (first pattern) (omd-entity.name entity))
               (let ((bind (omd=attribute.match* (second pattern) (omd-entity.attribute 
entity) bindings)))
                 (if (not (eq bind t))
                     (omd=entity.match* (third pattern) (omd-entity.content entity) bind)
                   t))
             t)
         t))
       (t (error "Unknown pattern type `~S' (this is a bug)" pattern)))))


(defun omd=entity.match* (pattern entity-list &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=entity.pattern.type pattern)
     (:empty
      (if (endp entity-list) bindings
        t))
     (:variable
      (let ((bind (omd=lookup-binding pattern bindings)))
        (if bind t
         (omd=add-binding pattern entity-list bindings))))
     (:list
      (let ((bind (omd=entity.match (first pattern) (first entity-list) bindings)))
        (if (not (eq bind t))
           (omd=entity.match* (rest pattern) (rest entity-list) bind)
         t)))))


(defun omd=attribute.match (pattern attribute &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=attribute.pattern.type pattern)
     (:variable
      (let ((bind (omd=lookup-binding pattern bindings)))
        (cond ((not bind)
              (omd=add-binding pattern attribute bindings))
             ((omd-attribute.are.equal (cdr bind) attribute)
              bindings)
             (t t))
        ))
     (:pattern
      (cond ((string-equal (first pattern) (omd-attribute.name attribute))
            (let ((bind (omd=lookup-binding (second pattern) bindings)))
              (cond ((not bind)
                     (omd=add-binding (second pattern) (omd-attribute.content attribute) bindings))
                    ((string-equal (cdr bind) (omd-attribute.content attribute))
                     bindings)
                    (t t))))
           (t t)))))

(defun omd=attribute.match* (pattern attr-set &optional (bindings '()))

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=attribute.set.pattern.type pattern)
     (:variable
      (let ((bind (omd=lookup-binding pattern bindings)))
        (cond ((not bind)
              (omd=add-binding pattern attr-set bindings))
             ((and (= (length (cdr bind)) (length attr-set))
                   (subsetp (cdr bind) attr-set :test #'omd-attribute.are.equal)
                   (subsetp attr-set (cdr bind) :test #'omd-attribute.are.equal))
              bindings)
             (t t))))
     (:empty bindings)
     (:list
      (let ((attr (find-if #'(lambda (attribute)
                              (not (eq t (omd=attribute.match (first pattern) attribute bindings))))
                          attr-set)))
        (cond (attr
              (omd=attribute.match* (rest pattern)
                                    (remove attr attr-set)
                                    (omd=attribute.match (first pattern) attr bindings)))
             (t t))))))


(defun omd=lookup-binding (var bindings)
   (assoc var bindings))

(defun omd=add-binding (key value bindings)
   (acons key value bindings))


(defun omd=entity.pattern.type (x)
   (cond ((stringp x)                            :literal)
        ((and (symbolp x)
              x)                                :variable)
        ((consp x)
         (cond ((and (= 3 (length x))
                     (stringp (first x))) :application)
               (t (error "Invalid pattern `~S'" x))))))

(defun omd=entity-list.pattern.type (x)

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



(defun omd=attribute.pattern.type (x)
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


(defun omd=attribute.set.pattern.type (x)

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



(defun omd-entity.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (remove-duplicates (omd=entity.pattern.free.vars pattern)))

(defun omd-attribute.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (remove-duplicates (omd=attribute.pattern.free.vars pattern)))


(defun omd=entity.pattern.free.vars (pattern)
   (case (omd=entity.pattern.type pattern)
     (:variable    (list pattern))
     (:literal     '())
     (:application (append
                   (omd=attribute.set.free.vars (second pattern))
                   (omd=entity.list.pattern.free.vars (third pattern))))))


(defun omd=entity.list.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=entity-list.pattern.type pattern)
     (:variable (list pattern))
     (:empty    '())
     (:list     (append (omd=entity.pattern.free.vars (car pattern))
                       (omd=entity.list.pattern.free.vars (cdr pattern))))))


(defun omd=attribute.pattern.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=attribute.pattern.type pattern)
     (:variable (list pattern))
     (:pattern  (list (second pattern)))))


(defun omd=attribute.set.free.vars (pattern)

   ;;; Edited  : 17. Jan 2002
   ;;; Authors : serge
   ;;; Input   :
   ;;; Effect  :
   ;;; Value   :

   (case (omd=attribute.set.pattern.type pattern)
     (:variable (list pattern))
     (:empty    '())
     (:list     (append (omd=attribute.pattern.free.vars (car pattern))
                       (omd=attribute.set.free.vars (rest pattern))))))


(defun omd-runParser (aString)
  (if (not (compiled-function-p #'xml-rpc::rpc=string-convert2xml))
      (compile 'xml-rpc::rpc=string-convert2xml))
  (let ((thisString (xml-rpc::rpc=string-convert2xml aString)))
    (inka::inka-trace "Parsing OMDoc...~%")
    (omd-xml2struct thisString)))

(defun omd-xml2struct (xmlstring)

  ;;; Edited  : 30. May 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (omd=buildStruct (omd=tokenize xmlstring)))

;;;Edited   : 1.2002
;;;Authors  : shaoning
;;;Input    :
;;;Effect   : extracting elements as tokens from OMDOC(string) 
;;;Value    : 
(defun omd=tokenize (aString)
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

(defun omd=attr2list (aString)
  (if (equal aString nil)
      nil
    (omd=attrTokenize aString "" nil)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun omd=attrTokenize (aString prefix result)
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
      (omd=attrTokenize (subseq aString (+ posDD 1)) (concatenate 'string prefix (subseq aString 0 (+ posDD 1))) result)))
   ((eq (aref aString 0) #\=)
    (let ((posS (position #\space (string-left-trim " " prefix) :from-end T)))
      (if posS
	  (omd=attrTokenize (concatenate 'string (subseq prefix (+ posS 1)) aString) "" (cons (subseq prefix 0 posS) result))
	(omd=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))
   ((eq (aref aString 0) #\space)
    (if (equal prefix "")
	(omd=attrTokenize (subseq aString 1) "" result)
      (progn 
	(assert (listp result))
	(omd=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))
   (t (omd=attrTokenize (subseq aString 1) (concatenate 'string prefix (subseq aString 0 1)) result))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun omd=buildAttr (aList)
  (if (equal aList nil)
      nil
    (cons (make-omd*attribute :name 
			      (subseq (car aList) 
				      0 (position #\= (car aList)))
			      :content 
			      ;(omd=getThemOut (omd=ascII2char (subseq (car aList) 
								     ;(+ 1 (position #\= (car aList)))))))
			      (omd=getThemOut (subseq (car aList)
						      (+ 1 (position #\= (car aList))))))
	  (omd=buildAttr (cdr aList)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun omd=buildStruct (aList)
  (let (value pos)
    (cond
     ((equal aList nil)
      nil)
     ((equal (length aList) 1)
      ;(list (omd=getThemOut (omd=ascII2char (car aList)))))
      (list (omd=getThemOut (car aList))))
     ((omd=isComment? (car aList))
      (omd=buildStruct (cdr aList)))
     (t
      (if (setf value (omd=isEntity? (car aList)))
	  (if (setf pos (omd=findTag aList value))
	      (if (equal pos (- (length aList) 1))
		  (list (make-omd*entity :name value
					 :attribute (omd=buildAttr (omd=attr2list (omd=getAttr (car aList))))
					 :content (omd=buildStruct (subseq aList 1 pos))))
		(append (omd=buildStruct (subseq aList 0 (+ pos 1)))
			(omd=buildStruct (subseq aList (+ pos 1)))))
	    (ERROR "No endtag for ~A in ~A" value aList))
	(omd=buildStruct (cdr aList)))))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun omd=isComment? (aString)
 (if (omd=isEntity? aString)
     (or
      (and 
       (equal (aref aString 1) #\?)
       (equal (aref aString (- (length aString) 2)) #\?))
      (equal (aref aString 1) #\!))
   nil))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
  
(defun omd=findTag (aList anEntity)
  (omd=findTagHelp (cdr aList) (string-right-trim '(#\Space #\Tab #\Newline) anEntity) 1 0 (car aList)))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
   

(defun omd=findTagHelp (aList anEntity counter position last)
  (cond
   ((eq counter 0) 
    position)
   ((equal aList nil)
    nil)
   ((omd=tagClose? (car aList) anEntity last)
    (omd=findTagHelp (cdr aList) anEntity (- counter 1) (+ position 1) (car aList)))
   ((omd=tagOpen? (car aList) anEntity)
    (omd=findTagHelp (cdr aList) anEntity (+ counter 1) (+ position 1) (car aList)))
   (t 
    (omd=findTagHelp (cdr aList) anEntity counter (+ position 1) (car aList)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : there strings
;;;Effect   : check if the first one(object) is in form of "</anEntity>" or if object "/>" is and the same time (omd=tagOpen? last anEntity) T returns
;;;Value    : if yes T others nil

(defun omd=tagClose? (object anEntity last)
  (or 
   (string-equal (concatenate 'string "</" (concatenate 'string anEntity ">")) 
		 object) 
   (and (omd=tagOpen? last anEntity) 
	(string-equal "/>" object))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : two strings
;;;Effect   : check if the first one(object) is in one form of "<anEntity>" or "<anEntity"   
;;;Value    : if yes T others nil
   

(defun omd=tagOpen? (object anEntity)
  (or
   (string-equal (concatenate 'string "<" (concatenate 'string anEntity ">")) 
		 object)
   (and (<= (+ (length anEntity) 2) (length object))
	(string-equal (concatenate 'string "<" (concatenate 'string anEntity " ")) 
		      (subseq object 0 (+ (length anEntity) 2))))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : a string
;;;Effect   : check if this string in form "<...>" is
;;;Value    : if yes T others nil

(defun omd=isEntity? (aString)
  (if  
      (and (equal (aref aString 0) #\<)
	   (not (equal (aref aString 1) #\/)))
      (if (position #\space aString)
	  (subseq aString 1 (position #\space aString))
	(if (equal (aref aString (- (length aString) 1)) #\>)
	    (subseq aString 1 (- (length aString) 1))
	  (subseq aString 1)))
    nil))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :

(defun omd=getAttr (aString)
  (let ((pos (position #\space aString)))
    (if (equal pos nil)
	nil
      (subseq aString (+ pos 1) (position #\> aString)))))

;;;Edited   : 1.2002
;;;Authors  : Shaoning
;;;Input    : 
;;;Effect   : 
;;;Value    :
  
(defun omd=getThemOut (aString)
  (string-trim "\"" aString))





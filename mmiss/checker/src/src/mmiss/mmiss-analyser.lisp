(in-package :DGRL)

(defun mmiss-checker-reset ()

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (handler-case 
      (multiple-value-bind (filename checks)
	  (mmiss=parse.command.line.args)
	(if (and (stringp filename)
		 checks
		 (subsetp checks *mmiss-checks*))
	    (let ((results (mmiss=file.check filename checks)))
	      (inka::mapcf #'(lambda (check result)
			       (mmiss-print "~A : ~A.~%" (string-downcase (string check)) (if result "ok" "fail")))
			   results))
	  (mmiss-print "~%Usage: mmiss-checker filename {~{~A~^|~}}+~%~%" 
		       (mapcar #'(lambda (symbol) (string-downcase (string symbol))) *mmiss-checks*))
	  )
	)
    (error (c) nil))
  (inka::pro-quit)
  )


(defvar *mmiss-output-stream* t)

(defvar *mmiss-checks* '(references acyclic-definitions))

(defvar *mmiss-debug* nil)

(defun mmiss-print (formatstring &rest args)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (apply 'format (cons *mmiss-output-stream* (cons formatstring args))))


(defun mmiss-error (formatstring &rest args)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if *mmiss-debug* 
      (apply 'error (cons formatstring args))
    nil))


(defun mmiss=parse.command.line.args ()

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : /
  ;;; Effect  : /
  ;;; Value   : a multiple value consisting of 1) the filename and 2) the list of checks.

  (let ((com-args (inka::command-line-arguments)))
    (values (first com-args)
	    (mapcar #'(lambda (string) (intern (string-upcase string) :dgrl)) (rest com-args)))))


(defun mmiss=file.check (filename checks)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((entity (mmiss-read.from.file filename))
	(results nil))
    (if entity
	(progn 
	  (dolist (check (remove-duplicates checks))
	    (case check 
	      (references 
	       (setf (getf results :references) 
		     (mmiss=check.required.references entity)))
	      (acyclic-definitions
	       (setf (getf results :acyclic-definitions)
		     (mmiss=check.acyclic.definitions entity)))
	      (T nil)))
	  results)
      nil)))


;;; =========================================================
;;; Parsing the XML files
;;; =========================================================


(defun mmiss-read.from.file (filename)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : a string denoting the file
  ;;; Effect  : reads the xml content of the file and returns
  ;;;           an omd*entity (dgrl package)
  ;;; Value   : see effect.

  (if (probe-file filename)
      (handler-case 
	  (first 
	   (omd-xml2struct 
	    (with-output-to-string 
	      (outstream)
	      (with-open-file 
		  (instream filename :direction :input)
		(let ((line (read-line instream nil :eof)))
		  (inka::while (inka::neq line :EOF)
			       (write-string line outstream)
			       (setq line (read-line instream nil :eof))))))))
	(error (c) 
	       (mmiss-print "Error parsing XML content of file ~A~%~A~%" filename c)
	       nil))
    (mmiss-print "Unknown file ~A~%" filename)
    ))



;;; =========================================================
;;; Finding definitions, references to definitions that 
;;; must be present.
;;; =========================================================


(defun mmiss=check.required.references (entity)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let* ((relevant-entities (mmiss=entity.find.definitions.and.references entity))
	 )
    (every #'(lambda (anentity)
	       (or (mmiss=entity.is.definitorial anentity)
		   (omd-entity.case
		    anentity 
		    (("link" (("status" status) ("linked" link)) content)
		     (declare (ignore status content))
		     (if 
			 (find-if #'(lambda (defentity)
				      (and (mmiss=entity.is.definitorial defentity)
					   (omd-entity.case 
					    defentity 
					    (("definition" (("label" label)) content)
					     (declare (ignore content))
					     (string-equal label link))
					    (X (declare (ignore X)) nil))))
				  relevant-entities)
			 t
		       (mmiss-print "Could not find definition required by ~A~%" anentity)))
		    (X (declare (ignore X)) nil))))
	   relevant-entities)))

    

(defun mmiss=entity.find.entities (entity closure &key (top-level t))

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (typep entity 'omd*entity)
      (if (funcall closure entity) 
	  (if top-level (list entity)
	    (cons entity (mapcan #'(lambda (subentity)
				     (mmiss=entity.find.entities subentity closure :top-level top-level))
				 (omd-entity.content entity))))
	(mapcan #'(lambda (subentity)
		    (mmiss=entity.find.entities subentity closure :top-level top-level))
		(omd-entity.content entity)))
    nil))


(defun mmiss=entity.find.definitions.and.references (entity)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : an omd*entity
  ;;; Effect  : /
  ;;; Value   : a list of all ids of objects defined in the entity.

  (mmiss=entity.find.entities
   entity
   #'(lambda (anentity)
       (or (mmiss=entity.is.definitorial anentity)
	   (mmiss=entity.is.required.reference anentity)))
   :top-level t))


(defun mmiss=entity.is.definitorial (entity)

  ;;; Edited  : 27. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (omd-entity.case 
   entity
   (("definition" attributes content)
    (declare (ignore attributes content))
    T)
   (X (declare (ignore X)) nil)))


(defun mmiss=entity.definition.get.name (entity)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (omd-entity.case entity
		   (("definition" (("label" label)) content)
		    (declare (ignore content))
		    label)
		   (X (declare (ignore X)) 
		      (mmiss-error "Entity ~A is not a defining entity!" entity))))



(defun mmiss=entity.is.required.reference (entity)

  ;;; Edited  : 27. Aug 2003 28. Aug 2003
  ;;; Authors : serge        serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (mmiss=entity.is.reference entity :required-status-p t))


(defun mmiss=entity.is.reference (entity &key (required-status-p nil))

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if required-status-p
      (omd-entity.case 
       entity
       (("link" (("status" status))  content) 
	(declare (ignore content))
	(if (string-equal status "present")
	    T
	  nil))
       (X (declare (ignore X)) NIL))
    (omd-entity.case 
     entity
     (("link" attr  content) 
	(declare (ignore attr content))
	T)
     (X (declare (ignore X)) NIL))
    ))


(defun mmiss=entity.reference.get.name (entity)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (omd-entity.case entity
		   (("link" (("linked" link)) content)
		    (declare (ignore content))
		    link)
		   (X (declare (ignore X)) 
		      (mmiss-error "Entity ~A is not a reference!" entity))))


;;; =========================================================
;;; Finding definitions, references to definitions that 
;;; must be present.
;;; =========================================================


(defun mmiss=check.acyclic.definitions (entity)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let* ((all-defs (mmiss=entity.find.entities
		    entity 'mmiss=entity.is.definitorial
		    :top-level t))
	 (defs+links (mapcar #'(lambda (defentity)
				 (let ((all-links 
					(mapcar 'mmiss=entity.reference.get.name 
						(mmiss=entity.find.entities
						 defentity 'mmiss=entity.is.reference
						 :top-level t))))
				   (cons (mmiss=entity.definition.get.name defentity) all-links)))
			     all-defs))
	 (cycles (if defs+links
		     (let ((min-defname.linknames-list (mmiss=defnames+links.minimal.entries defs+links)))
		       (if min-defname.linknames-list
			   (mapcan #'(lambda (min-defname.linknames)
				       (mmiss=defnames+links.find.cycles min-defname.linknames 
									 defs+links nil))
				   min-defname.linknames-list)
			 (mapcan #'(lambda (min-defname.linknames)
				     (mmiss=defnames+links.find.cycles min-defname.linknames 
								       defs+links nil))
				 defs+links)))
		   nil)))
    (when cycles 
      (mmiss-print "~%")
      (dolist (cycle cycles)
	(mmiss-print "Detected cycle ~{~A~^ -> ~}~%" cycle)))
    (null cycles)
    ))


(defun mmiss=defnames+links.find.cycles (defname.linknames defname.linknames-list &optional visited-defs)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (null defname.linknames-list) nil
    (let ((pos (position (car defname.linknames) visited-defs :test #'string-equal)))
      (if pos
	  (list (append (subseq visited-defs pos) (list (car defname.linknames))))
	(mapcan #'(lambda (succ-defname.linknames)
		    (mmiss=defnames+links.find.cycles succ-defname.linknames defname.linknames-list 
						      (append visited-defs (list (car defname.linknames)))))
		(inka::find-all #'(lambda (defname1.linknames1)
			      (member (car defname1.linknames1) 
				      (cdr defname.linknames)
				      :test #'string-equal))
			  defname.linknames-list))))))

    
    

(defun mmiss=defnames+links.minimal.entries (defname.linknames-list)

  ;;; Edited  : 28. Aug 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (inka::find-all #'(lambda (defname.linknames)
		      (every #'(lambda (defname1.linknames1)
				 (not (member (first defname.linknames)
					      (rest defname1.linknames1)
					      :test #'string-equal)))
			     (remove defname.linknames defname.linknames-list)))
		  defname.linknames-list))



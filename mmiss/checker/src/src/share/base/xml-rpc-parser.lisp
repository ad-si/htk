;;; -*- syntax: common-lisp; package: xml-rpc; base: 10; mode: lisp -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :xml-rpc)
    (defpackage :xml-rpc
      (:use :cl)
      )
    ))

(in-package :xml-rpc)

(defparameter rpc*methodcall "methodCall")
(defparameter rpc*methodname "methodName")
(defparameter rpc*params "params")
(defparameter rpc*param "param")

(defparameter rpc*value "value")
(defparameter rpc*int "int")
(defparameter rpc*i4 "i4")
(defparameter rpc*bool "boolean")
(defparameter rpc*string "string")
(defparameter rpc*double "double")
(defparameter rpc*base64 "base64")

(defparameter rpc*struct "struct")
(defparameter rpc*member "member")
(defparameter rpc*name   "name")

(defparameter rpc*array "array")
(defparameter rpc*data "data")

(defparameter rpc*methodresponse "methodResponse")
(defparameter rpc*fault "fault")

(defparameter rpc*xml "<?xml version=\"1.0\"?>")

;;; Parsing RPC

(defun rpc=parse-error (strng)
    (error "Parse error in ~A" strng))


(defun rpc=rpc2list (strng &optional (entityname rpc*methodcall))
  (multiple-value-bind (begin endpos) (rpc=rpc2list.intern strng entityname)
    (if (and begin endpos)
	(http::http~html2list (subseq strng begin (- (length strng) endpos)))
      (rpc=parse-error strng))))


(defun rpc=list2xmlstring (alist)

  ;;; Edited  : 29. Jun 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (with-output-to-string (output)
	(rpc=list2xmlstring.1 alist output)))

(defun rpc=list2xmlstring.1 (alist output)

  ;;; Edited  : 29. Jun 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (cond ((atom alist) (write-string (string alist) output))
	((stringp alist) (write-string alist output))
	((and (listp alist) (atom (first alist)))
	 (write-string (http::http~open (string (first alist))) output)
	 (dolist (arg (rest alist)) (rpc=list2xmlstring.1 arg output))
	 (write-string (http::http~close (string (first alist))) output))
	((and (listp alist) (listp (first alist)))
	 (dolist (arg alist) (rpc=list2xmlstring.1 arg output)))
	(t (error "Unknown list-structure ~A~%" alist))))

	 
	 
	 

(defun rpc=rpc2list.intern (strng entityname)

  ;;; Edited  : 17. May 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 
  
  (let ((begin (search (http::http~open entityname) strng :key #'char-downcase))
	(endpos (search (reverse (http::http~close entityname)) (reverse strng) :key #'char-downcase)))
    (values begin endpos)))



(defun rpc~parse-methodcall (str)
  (let* ((liste (rpc=rpc2list str))
	 (name (second liste))
	 (args (third liste)))
    (if (and (consp name)(consp args))
	(cons (rpc=parse-name name)
	      (rpc=parse-params args))
    (rpc=parse-error (format nil "~A | ~A" name args)))))


(defun rpc~parse-methodresponse (str)

  ;;; Edited  : 05. Mar 2001 25. Oct 2002
  ;;; Authors : serge        serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 
	      
  (let* ((liste (rpc=rpc2list str rpc*methodresponse))
	 (args (second liste)))
    (cond ((string-equal (first args) rpc*fault)
	   (values (rpc=parse-fault args) t))
	  ((string-equal (first args) rpc*params)
	   (values (rpc=parse-params args) t))
	  (t (values (rpc=parse-error liste) nil)))))

    
(defun rpc=parse-fault (arg)

  ;;; Edited  : 05. Mar 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (and (string-equal (first arg) rpc*fault)
	   (string-equal (first (second arg)) rpc*value))
      (rpc=parse-value (second (second arg)))
    (rpc=parse-error arg)))


(defun rpc=parse-name (liste)
  (if (string-equal (car liste) rpc*methodname)
      (read-from-string (second liste))
    (rpc=parse-error liste)))

(defun rpc=parse-params (liste)
  (if (string-equal (car liste) rpc*params)
      (mapcar #'(lambda (para)
		  (if (string-equal (car para) rpc*param)
		      (rpc=parse-param (second para))
		    (rpc=parse-error para))) 
	      (rest liste))
    (rpc=parse-error liste)))

(defun rpc=parse-param (arg)
  (cond ((string-equal (car arg) rpc*value)
	 (rpc=parse-value (car (last arg))))
	((string-equal (car arg) rpc*struct)
	 (rpc=parse-struct (car (last arg))))
	((string-equal (car arg) rpc*array)
	 (rpc=parse-array (car (last arg))))
	(T (rpc=parse-error arg))))
	
(defun rpc=parse-value (arg)
  (cond
   ((atom arg)
    (rpc=parse-string arg))
   ((string-equal (car arg) rpc*string)
    (rpc=parse-string (second arg)))
   ((or (string-equal (car arg) rpc*double)
	(string-equal (car arg) rpc*int)
	(string-equal (car arg) rpc*i4))
    (read-from-string (second arg)))
   ((string-equal (car arg) rpc*bool)
    (if (zerop (read-from-string (second arg))) nil T))
   ((string-equal (car arg) rpc*struct)
    (rpc=parse-struct (rest arg)))
   ((string-equal (car arg) rpc*array)
    (rpc=parse-array (second arg)))
   (T ;; (rpc=parse-error arg)
    (rpc=list2xmlstring arg))))



(defun rpc=parse-string (arg)
  (let ((sanitized-string (map 'string #'(lambda (char)
					   (if (member char '(#\tab #\newline #\return #\linefeed))
					       #\space char))
			       arg)))
    (let ((converted-string 
	   (rpc=string-convert2xml sanitized-string)))
      (string-trim '(#\space) converted-string))))

(defun rpc=string-convert2xml (arg &optional start stream)

  ;;; Edited  : 29. Oct 2002 15. Nov 2002
  ;;; Authors : serge        serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (if (and start stream)
      (let ((length-arg (length arg)))
	(loop while (<= start length-arg) do
	  (multiple-value-bind (prefix-endpos char reststring-startpos) 
	      (rpc=string-find-next-code arg start)
	    (cond (char 
		   (write-string arg stream :start start :end prefix-endpos)
		   (write-char char stream)
		   (setq start reststring-startpos)
		   ;; (rpc=string-convert2xml arg reststring-startpos stream)
		   )
		  (t (write-string arg stream :start start)
		     (setq start (1+ length-arg))
		     )))))
    (with-output-to-string 
      (stream)
      (rpc=string-convert2xml arg 0 stream))))


(defun rpc=string-find-next-code (arg start)

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


(defun rpc=parse-struct (arg)
  (let ((aconslist))
    (mapc #'(lambda (members)
	      (if (string-equal (car members) rpc*member)
		    (let ((name (second members))
			  (value (third members)))
		      (if (and (string-equal (car name) rpc*name)
			       (string-equal (car value) rpc*value))
			  (setq aconslist 
				(acons (read-from-string (second name))
				       (rpc=parse-value (second value)) aconslist))
			(rpc=parse-error (format nil "~A | ~A" name value))))
		  (rpc=parse-error members)))
	    arg)
    aconslist))

		
(defun rpc=parse-array (arg)
  (let ((datalist (rest arg)))
    (if (string-equal (car arg) rpc*data)
	  (mapcar #'(lambda (members)
		    (if (string-equal (car members) rpc*value)
			(rpc=parse-value (second members))
		      (rpc=parse-error members)))
		  datalist)
      (rpc=parse-error  arg))))

;;; Composing RPC


(defun rpc~compose-methodresponse (params &key (fault nil))
  (let ((tags (if fault (list rpc*methodresponse rpc*fault)
		(list rpc*methodresponse rpc*params))))
    (concatenate 'string
		 rpc*xml 
		 (http::http~tag tags
			   (mapcar #'(lambda (para) (http::http~tag rpc*param (rpc=value para))) params)))))

(defun rpc~compose-methodcall (command params)
    (concatenate 'string
		 rpc*xml 
		 (http::http~tag rpc*methodcall
			   (list (http::http~tag rpc*methodname command)
				 (http::http~tag rpc*params
					   (if params
					     (mapcar #'(lambda (para) (http::http~tag rpc*param (rpc=value para))) params)
					     ""))))))

(defun rpc=value (thing) 
  (http::http~tag rpc*value (rpc=value-types thing)))

(defgeneric rpc=value-types (thing)
  (:method ((thing string))
	   (http::http~tag rpc*string thing))
  (:method ((thing cons))
	   (if (and (every #'consp thing)  ;;an alist
		    (every #'(lambda (pair) (not (listp (rest pair)))) thing))
	       (http::http~tag rpc*struct
			 (mapcan #'(lambda (x)
				  (list (http::http~tag rpc*name (car x))
					(rpc=value (rest x)))) thing))
	   (http::http~tag (list rpc*array rpc*data)
		     (mapcar #'rpc=value thing))))
  (:method ((thing integer))
	   (http::http~tag rpc*int
		    (format nil "~A" thing)))
  (:method ((thing (eql T)))
	   (http::http~tag rpc*bool "1"))
  (:method ((thing (eql nil)))
	   (http::http~tag rpc*bool "0"))
  (:method (thing)
	   (http::http~tag rpc*string (format nil "~A" thing)))
  )





;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA 
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;

;; $Id$

;; Change Log 
;;
;; 10/14/00 add namespace support; xml-error fix

(in-package :net.xml.parser)



(defun intern* (string length package)
  (declare (simple-string string))
  (intern (subseq string 0 length) package))



(defun xml-error (control &rest args)
  (apply #'error (format nil "XML not well-formed - ~A" control) args))



(defparameter *collectors* (list nil nil nil nil nil nil nil nil))

;;; Save the collector 'col in the *collectors* list if there is room,
;;; else toss it away.
(defun put-back-collector (col)
  (do ((cols *collectors* (cdr cols)))
      ((null cols))
    (unless (or (car cols)
		(kernel:rplaca-conditional cols nil col))
      (return))))

(defun pub-id-char-p (char)
  (declare (character char))
  (let ((code (char-code char)))
    (or (= #x20 code) (= #xD code) (= #xA code)
	(<= (char-code #\a) code (char-code #\z))
	(<= (char-code #\A) code (char-code #\Z))
	(<= (char-code #\0) code (char-code #\9))
	(member char '( #\- #\' #\( #\) #\+ #\, #\. #\/ #\: #\= #\?
		       #\; #\! #\* #\# #\@ #\$ #\_ #\%)))))

(defparameter *keyword-package* (find-package :keyword))

;; cache of tokenbuf structs
(defparameter *tokenbufs* (list nil nil nil nil))

(defstruct tokenbuf
  ;; next index to use to grab from tokenbuf
  (cur 0 :type fixnum)
  ;; index one beyond last character
  (max 0 :type fixnum)
  ;; character array
  (data (ext:required-argument) :type simple-string)
  ;; for external sources
  (stream nil :type (or null stream)))


(defstruct iostruct
  ;; character pushed back
  (unget-char nil :type list)
  ;; main input tokenbuf
  (tokenbuf nil :type (or null tokenbuf))
  ;; optional alternative to read-sequence
  (read-sequence-func nil :type (or null function))
  ;; active entity tokenbufs
  entity-bufs
  ;; active entity names
  entity-names
  parameter-entities
  general-entities
  ;; still substituting entity text
  do-entity
  seen-any-dtd
  seen-external-dtd
  seen-parameter-reference
  standalonep
  uri-to-package
  ns-to-package
  ns-scope
  )


(defun get-tokenbuf ()
  (do ((bufs *tokenbufs* (cdr bufs)))
      ((endp bufs)
       (make-tokenbuf :cur 0  :max 0
		      :data (make-array 1024 :element-type 'character)))
    (let ((buf (car bufs)))
      (when (and buf (eq (kernel:rplaca-conditional bufs buf nil) buf))
	(setf (tokenbuf-cur buf) 0)
	(setf (tokenbuf-max buf) 0)
	(setf (tokenbuf-stream buf) nil)
	(return buf)))))
      

(defstruct collector
  ;; next index to set
  (next 0 :type fixnum)
  ;; 1+max index to set
  (max 0 :type fixnum)
  ;; string vector
  (data (ext:required-argument) :type simple-string))


(defun compute-tag (coll &optional (package *keyword-package*) ns-to-package)
  ;; compute the symbol named by what's in the collector
  (if* (not ns-to-package)
     then (intern* (collector-data coll) (collector-next coll) package)
     else
	  (let (new-package (data (collector-data coll)))
	    (if* (and (eq (schar data 0) #\x)
		      (eq (schar data 1) #\m)
		      (eq (schar data 2) #\l)
		      (eq (schar data 3) #\n)
		      (eq (schar data 4) #\s)
		      (or (eq (schar data 5) #\:)
			  (= (collector-next coll) 5)))
	       then ;; putting xmlns: in :none namespace
		    (setf new-package (assoc :none ns-to-package))
		    (when new-package (setf package (rest new-package)))
		    (intern* (collector-data coll) (collector-next coll)
			     package)
	       else
		    (let ((colon-index -1)
			  (data (collector-data coll)))
		      (dotimes (i (collector-next coll))
			(when (eq (schar data i) #\:)
			  (setf colon-index i)
			  (return)))
		      (if* (> colon-index -1) then
			      (let ((string1 (make-string colon-index))
				    new-package string2)
				(dotimes (i colon-index)
				  (setf (schar string1 i) (schar data i)))
				(setf new-package (assoc string1 ns-to-package :test 'string=))
				(if* new-package
				   then
					(setf string2 (make-string (- (collector-next coll)
								      (+ 1 colon-index))))
					(dotimes (i (- (collector-next coll)
						       (+ 1 colon-index)))
					  (setf (schar string2 i) 
					    (schar data (+ colon-index 1 i))))
					(intern string2 (cdr new-package))
				   else
					(intern* (collector-data coll) 
						 (collector-next coll)
						 package)))
			 else
			      (let ((new-package (assoc :none ns-to-package)))
				(when new-package
				  (setf package (rest new-package))))
			      (intern* (collector-data coll) 
				       (collector-next coll) package)))
		    ))
	  ))

(defun compute-coll-string (coll)
  (declare (type collector coll))
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))
    
    str))

(defun grow-and-add (coll ch)
  (declare (type collector coll)
	   (character ch))
  ;; increase the size of the data portion of the collector and then
  ;; add the given char at the end
  (let* ((odata (collector-data coll))
	 (ndata (make-string (* 2 (length odata)))))
    (dotimes (i (length odata))
      (setf (schar ndata i) (schar odata i)))
    (setf (collector-data coll) ndata)
    (setf (collector-max coll) (length ndata))
    (let ((next (collector-next coll)))
      (setf (schar ndata next) ch)
      (setf (collector-next coll) (1+ next)))))

;;; Save the buffer 'buf in the *tokenbufs* list if there is room,
;;; else toss it away.
(defun put-back-tokenbuf (buf)
  (do ((bufs *tokenbufs* (cdr bufs)))
      ((null bufs))
    (unless (or (car bufs)
		(kernel:rplaca-conditional bufs nil buf))
      (return))))

(defun get-collector ()
  (do ((cols *collectors* (cdr cols)))
      ((null cols)
       (make-collector :next 0  :max 100  :data (make-string 100)))
    (let ((col (car cols)))
      (when (and col (eq (kernel:rplaca-conditional cols col nil) col))
	(setf (collector-next col) 0)
	(return col)))))

(defmacro next-char (tokenbuf read-sequence-func)
  (ext:once-only ((read-sequence-func read-sequence-func))
    `(let ((cur (tokenbuf-cur ,tokenbuf))
	   (tb (tokenbuf-data ,tokenbuf)))
      (when (>= cur (tokenbuf-max ,tokenbuf))
	;; fill buffer
	(if (or (not (tokenbuf-stream ,tokenbuf))
		(zerop (setf (tokenbuf-max ,tokenbuf)
			     (if ,read-sequence-func
				 (funcall ,read-sequence-func tb 
					  (tokenbuf-stream ,tokenbuf))
				 (read-sequence tb (tokenbuf-stream ,tokenbuf))))))
	    (setq cur nil)	;; eof
	    (setq cur 0)))
      (when cur
	(prog1
	    (let ((cc (schar tb cur)))
	      (if (and (tokenbuf-stream ,tokenbuf) (eq #\return cc))
		  #\newline
		  cc))
	  (setf (tokenbuf-cur ,tokenbuf) (1+ cur)))))))

(defun get-next-char (iostruct)
  (let* ((from-stream nil)
	 (tmp-char
	  (let (char)
	   (if* (iostruct-unget-char iostruct) then
		   ;; from-stream is used to do input CR/LF normalization
		   (setf from-stream t)
		   (setf char (first (iostruct-unget-char iostruct)))
		   (setf (iostruct-unget-char iostruct) (rest (iostruct-unget-char iostruct)))
		   char
	    elseif (iostruct-entity-bufs iostruct) then
		   (let (entity-buf)
		     (loop
		       (setf entity-buf (first (iostruct-entity-bufs iostruct)))
		       (if* (streamp (tokenbuf-stream entity-buf))
			  then (setf from-stream t)
			  else (setf from-stream nil))
		       (setf char (next-char entity-buf (iostruct-read-sequence-func iostruct)))
		       (when char (return))
		       (when (streamp (tokenbuf-stream entity-buf))
			 (close (tokenbuf-stream entity-buf))
			 (put-back-tokenbuf entity-buf))
		       (setf (iostruct-entity-bufs iostruct) (rest (iostruct-entity-bufs iostruct)))
		       (setf (iostruct-entity-names iostruct) (rest (iostruct-entity-names iostruct)))
		       (when (not (iostruct-entity-bufs iostruct)) (return))))
		   (if* char then char
		      else (next-char (iostruct-tokenbuf iostruct) 
				      (iostruct-read-sequence-func iostruct)))
	      else (setf from-stream t)
		   (next-char (iostruct-tokenbuf iostruct) 
			      (iostruct-read-sequence-func iostruct))))))
    (if* (and from-stream (eq tmp-char #\return)) then #\newline else tmp-char)))

(defun unicode-check (p tokenbuf)
  (declare (type stream p)
	   (ignorable tokenbuf))
  (let* ((c (read-char p nil)) c2
	 (c-code (if c (char-code c) nil)))
    (cond ((eq #xFF c-code)
	   (setf c2 (read-char p nil))
	   (setf c-code (if c (char-code c2) nil))
	   (cond ((eq #xFE c-code)
		  (xml-error "Unicode unsupported.")
		  #+nil
		  (setf (stream-external-format p)
			(find-external-format :unicode)))
		 (t
		  (xml-error "stream has incomplete Unicode marker"))))
	  (t
	   #+nil
	   (setf (stream-external-format p)
		 (find-external-format :utf8))
	   (when c
	     (push c (iostruct-unget-char tokenbuf))
	     #+ignore (unread-char c p)  ;; bug when there is single ^M in file
	     )))))


(defun add-default-values (val attlist-data)
  (if* (symbolp val)
     then
	  (let* ((tag-defaults (assoc val attlist-data)) defaults)
	    (dolist (def (rest tag-defaults))
	      (if* (stringp (third def)) then
		      (push (first def) defaults)
		      (push (if (eq (second def) :CDATA) (third def) 
			      (normalize-attrib-value (third def))) defaults)
	       elseif (and (eq (third def) :FIXED) (stringp (fourth def))) then
		      (push (first def) defaults)
		      (push (if (eq (second def) :CDATA) (fourth def) 
			      (normalize-attrib-value (fourth def))) defaults)
		      ))
	    (if* defaults then
		    (setf val (append (list val) (nreverse defaults)))
	       else val)
	    )
     else
	  ;; first make sure there are no errors in given list
	  (let ((pairs (rest val)))
	    (loop
	      (when (null pairs) (return))
	      (let ((this-one (first pairs)))
		(setf pairs (rest (rest pairs)))
		(when (member this-one pairs)
		  (xml-error "Entity: ~A has multiple ~A attribute values."
			     (string (first val)) (string this-one))))))
	  (let ((tag-defaults (assoc (first val) attlist-data)) defaults)
	    (dolist (def (rest tag-defaults))
	      (let ((old (member (first def) (rest val))))
		(if* (not old) then
			(if* (stringp (third def)) then
				(push (first def) defaults)
				(push (third def) defaults)
			 elseif (and (eq (third def) :FIXED) (stringp (fourth def))) then
				(push (first def) defaults)
				(push (fourth def) defaults))
		   else
			(push (first old) defaults)
			(push (second old) defaults))))
	    (if* defaults then
		    ;; now look for attributes in original list that weren't in dtd
		    (let ((tmp-val (rest val)) att att-val)
		      (loop
			(when (null tmp-val) (return))
			(setf att (first tmp-val))
			(setf att-val (second tmp-val))
			(setf tmp-val (rest (rest tmp-val)))
			(when (not (member att defaults))
			  (push att defaults)
			  (push att-val defaults))))
		    (setf val (append (list (first val)) (nreverse defaults)))
	       else val))
	  ))

(defun normalize-public-value (public-value)
  (setf public-value (string-trim '(#\space) public-value))
  (let ((count 0)
	(stop (length public-value))
	(last-ch nil)
	cch)
    (declare (fixnum count stop))
    (loop
     (when (= count stop)
       (return public-value))
     (setf cch (schar public-value count))
     (cond ((and (eq cch #\space) (eq last-ch #\space))
	    (setf public-value 
		  (remove #\space public-value :start count :count 1))
	    (decf stop))
	   (t
	    (incf count)
	    (setf last-ch cch))))))
  

(defun normalize-attrib-value (attrib-value &optional first-pass)
  (declare (string attrib-value))
  (when first-pass
    (let ((count 0)
	  (stop (length attrib-value))
	  (last-ch nil)
	  cch)
      (declare (fixnum count stop))
      (loop
       (when (= count stop)
	 (return))
       (setf cch (schar attrib-value count))
       (cond ((or (eq cch #\return) (eq cch #\tab))
	      (setf (schar attrib-value count) #\space))
	     ((and (eq cch #\newline) (not (eq last-ch #\return)))
	      (setf (schar attrib-value count) #\space))
	     ((and (eq cch #\newline) (eq last-ch #\return))
	      (setf attrib-value 
		    (remove #\space attrib-value :start count :count 1))
	      (decf stop)))
       (incf count)
       (setf last-ch cch))))
  (setf attrib-value (string-trim '(#\space) attrib-value))
  (let ((count 0) (stop (length attrib-value)) (last-ch nil) cch)
    (declare (fixnum count stop))
    (loop
     (when (= count stop) (return attrib-value))
     (setf cch (schar attrib-value count))
     (cond ((and (eq cch #\space) (eq last-ch #\space))
	    (setf attrib-value 
		  (remove #\space attrib-value :start count :count 1))
	    (decf stop))
	   (t
	    (incf count)
	    (setf last-ch cch))))))

(defun check-xmldecl (val tokenbuf)
  (declare (type iostruct tokenbuf))
  (when (not (and (symbolp (second val)) (string= "version" (symbol-name (second val)))))
    (xml-error "XML declaration tag does not include correct 'version' attribute"))
  (when (and (fourth val) 
	     (or (not (symbolp (fourth val)))
		 (and (not (string= "standalone" (symbol-name (fourth val)))) 
		      (not (string= "encoding" (symbol-name (fourth val)))))))
    (xml-error "XML declaration tag does not include correct 'encoding' or 'standalone' attribute"))
  (when (and (fourth val) (string= "standalone" (symbol-name (fourth val))))
    (if* (equal (fifth val) "yes") then
	   (setf (iostruct-standalonep tokenbuf) t) 
     elseif (not (equal (fifth val) "no")) then
	    (xml-error "XML declaration tag does not include correct 'standalone' attribute value")))
  (dotimes (i (length (third val)))
    (let ((c (schar (third val) i)))
      (when (and (not (alpha-char-p c))
		 (not (digit-char-p c))
		 (not (member c '(#\. #\_ #\- #\:)))
		 )
	(xml-error "XML declaration tag does not include correct 'version' attribute value"))))
  (when (and (fourth val) (eql :encoding (fourth val)))
    (dotimes (i (length (fifth val)))
      (let ((c (schar (fifth val) i)))
	(when (and (not (alpha-char-p c))
		   (if* (> i 0) then
			   (and (not (digit-char-p c))
				(not (member c '(#\. #\_ #\-))))
		      else t))
	  (xml-error "XML declaration tag does not include correct 'encoding' attribute value")))))
  )

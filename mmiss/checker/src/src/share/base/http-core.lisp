;;; -*- syntax: common-lisp; package: http; base: 10; mode: lisp -*-
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
  (unless (find-package :http)
    (defpackage :http
      (:use :cl)
      )
    ))

(in-package :http)


;;; HTTP

;;; Variables for HTTP


(defparameter http*codes '(   200 "OK"
                             400 "Bad Request"
			     403 "Forbidden"
			     404 "Not Found"
			     301 "Moved Permanently" 
			     302 "Moved Temporarily" 
			     303 "See Other" ; HTTP 1.1 only 
			     500 "Server Error"
			     501 "Method Not Implemented"))

(defparameter http*days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter http*error-standard-text "<P><a href=\"mailto:pollet@ags.uni-sb.de\">Report your problem.</a>")

;;; Reading via HTTP

(defun http=date-string ()
  (multiple-value-bind (sec min hour day month year dayofweek daylightsaving zone)
      (get-decoded-time)
    (format nil "~A, ~A ~A ~A ~A:~A:~A GMT "
	    (elt http*days dayofweek) day month  year (+ hour zone (if daylightsaving -1 0)) min sec)))

(defun http=read-request (socketname)
  (let ((first-line (read-from-string (concatenate 'string "(" (inka::socket-read.line socketname) ")"))))
    (when (= (length first-line) 2) (append first-line (list 'http/1.0)))
    (if (= (length first-line) 3) first-line
      (error "request not wellformed"))))

(defun http=read-header (socketname)
  (do* ((line (http=headerline2list (inka::socket-read.line socketname))
	      (http=headerline2list (inka::socket-read.line socketname)))
        (result line (append line result)))
      ((null line) result)))

(defun http=headerline2list (string)
  (unless (string= (string-trim '(#\newline #\return #\linefeed) string) "")
    (let ((pos (search ":" string)))
      (list (string-trim '(#\space #\tab #\newline #\return #\linefeed) (subseq string 0 pos))
	    (string-trim '(#\space #\tab #\newline #\return #\linefeed) (subseq string (1+ pos) (length string)))))))

(defun http~read-page (socketname &optional (error-p t))
  (let* ((request (http=read-request socketname))
	 (header  (http=read-header socketname))
	 (pos-of-content-length (position-if #'(lambda (sym)
						 (string-equal sym "content-length"))
					     header)))
    (labels ((handle-error ()
			   (when error-p
			     (error "Header contains no content-length"))
			   (warn "Header contains no content-length")
			   (list request header)))
      (if pos-of-content-length
	  (let* ((pos-of-size  (1+ pos-of-content-length))
		 (size (read-from-string (nth  pos-of-size header))))
	    (if (numberp size)
		(list request header (inka::socket-read.content size socketname))
	      (handle-error)))
	(handle-error)))))

;;; Sending via HTTP


(defun http~send-error (socketname &optional (number 400))
  (let* ((error-string (getf http*codes number))
	 (error-text (format nil "~A ~A" number error-string))
	 (content (concatenate 'string
			       (format nil "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 2.0//EN\">~%")
			       (http~tag "HTML"
					 (list (http~tag (list "HEAD" "TITLE") error-text)
					       (http~tag "BODY" (list (http~tag "H1" error-text)
								      http*error-standard-text)))))))
    (progn
      (http~send-response socketname content 
			:status error-text))))

(defun http~send-file (socketname pathname &key (status "200 OK") (version "HTTP/1.0")
				  (type " text/html") (server "omega"))
  (let ((header (format nil "~A ~A~%Connection: close~%Content-Length: ~A~%Content-Type: ~A~%Date: ~A~%Server: ~A~%~%"
			  version status
			  (with-open-file (in pathname :direction :input
					      :element-type 'unsigned-byte
					      :if-not-exists :error-p)
					  (file-length in))
			  type (http=date-string) server)))
    (inka::socket-write.file header
		       (format nil "~A" pathname) ; pathname can be a string or a pathname
		       socketname)))
			
(defun http~send-response ( socketname content &key
				       (status "200 OK")(version "HTTP/1.0")(type " text/html")(server "omega"))
  (let ((header (format nil "~A ~A~%Connection: close~%Content-Length: ~A~%Content-Type: ~A~%Date: ~A~%Server: ~A~%~%"
			version status (length content) type (http=date-string) server)))
    (inka::socket-write (concatenate 'string header content) socketname "")))

(defun http~send-request (socketname content &key
				     (request "POST")(uri "")(version "HTTP/1.0")(type " text/html"))
  (let ((header (format nil "~A ~A ~A~%Content-Length: ~A~%Content-Type: ~A~%~%"
			request uri version (length content) type)))
    (inka::socket-write (concatenate 'string header content) socketname)))

;; Help for HTTP-tags



(defun http~next-tag (content &optional (act.pos 0))
  (let ((tagbegin (position #\< content :start act.pos)))
    (when tagbegin
      (let ((tagend (position #\> content :start tagbegin)))
	(if tagend
	    (values (subseq content tagbegin (1+ tagend))
		    tagbegin)
	  (error "html-tag in ~A not wellformed" content))))))


(defun http~find-tag (content tag)
  (let ((tagpos (search tag  content :test #'string-equal)))
     (if tagpos (+ tagpos (length tag)))))

(defun http~endtag? (tag)
  (and (string= "<" (elt tag 0))
       (string= "/" (elt tag 1))))

(defun http~name-of-tag (tag)
  (string-right-trim ">" (string-left-trim "</" tag)))

(defun http~open (tag)
  (format nil "<~A>" tag))

(defun http~close (tag)
  (format nil "</~A>"
	  (let ((pos (position #\space tag)))
	    (if pos (subseq tag 0 pos) tag))))

(defgeneric http~tag (tag content)
  (:method ((tag cons)(content t))
	   (http~tag (car tag)
	    (http~tag (rest tag)  content)))
  (:method ((tag string) (content t))
	   (format nil
		   "~A~@[~A~]~A"
		   (http~open tag)
		   (http~tag nil content)
		   (http~close tag)))
  (:method ((tag null) (content string))
	   content)
  (:method ((tag null) (content symbol))
	   (string content))
  (:method ((tag null) (content null)))
  (:method ((tag null) content)
	   (format nil "~A" content))
  (:method ((tag null) (content cons))
	   (concatenate 'string (http~tag tag (car content))
		        (http~tag tag (rest content)))))


(defun http~geturlnameporturi (url) ;;just a hack, MP!
  (let* ((newurl (if (string-equal "http" (subseq url 0 4))
		     (subseq url (+ 2 (search "//" url)))
		   url))
	 (pos-of-uri (search "/" newurl))
	 (hostport (if pos-of-uri (subseq newurl 0 pos-of-uri) newurl))
	 (uri (if pos-of-uri (subseq newurl pos-of-uri) "/"))
	 (pos-of-port (search ":" hostport)))
    (values
     (if pos-of-port (subseq hostport 0 pos-of-port) hostport)
     (if pos-of-port (read-from-string (subseq hostport (1+ pos-of-port))) nil)
     uri)))


(defun http~html2list (content)
  (let ((result 
	 (with-output-to-string (stream)
				(http=html2list-rec content stream))))
    (read-from-string result)))
  

(defun http=html2list-rec (content stream &optional (act.pos 0))
  (labels ((getcontent (strng)
		       (if (string= "" (string-trim '(#\space #\tab #\newline #\return #\linefeed) strng))
			   ""
			 (concatenate 'string " \"" (remove #\" strng) "\" "))))
  (multiple-value-bind (next-tag next-pos)    
      (http~next-tag content act.pos)
    (cond
     ((and next-tag
	   (http~endtag? next-tag))
      (write-string (getcontent (subseq content act.pos next-pos)) stream)
      (write-string ")" stream)
      ;; (http=html2list-rec (subseq content (+ next-pos (length next-tag))) stream)
      (http=html2list-rec content stream (+ next-pos (length next-tag)))
      )
     (next-tag
      (write-string (getcontent (subseq content act.pos next-pos)) stream)
      (write-string "(" stream)
      (write-string (http~name-of-tag next-tag) stream)
      ;; (http=html2list-rec (subseq content (+ next-pos (length next-tag))) stream)
      (http=html2list-rec content stream (+ next-pos (length next-tag)))
      )))))


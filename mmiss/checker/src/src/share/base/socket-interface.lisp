;;; -*- Mode: Lisp; Base: 10; Syntax: Common-lisp; Package: INKA -*-
;; 
;; ********************************************************************************************
;; *** This file is adapted from socket.lisp from the KEIM project written by Stephan Hess. ***
;; ********************************************************************************************
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 151150                                                      ;;
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

(in-package :inka)

(require "service")

;; ---------------------------------------------------------------------------
;; Section 0 : foreign function definitions for socket implementation
;; ---------------------------------------------------------------------------

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(progn 
  (ff:defforeign 'perror :arguments '(string) :pass-types '(:by-value))
  (ff:defforeign 'usocket)
  (ff:defforeign 'ubind :arguments '(integer) :pass-types '(:by-value))
  (ff:defforeign 'uaccept :arguments '(integer) :pass-types '(:by-value))
  (ff:defforeign 'uclose :arguments '(integer) :pass-types '(:by-value))
  (ff:defforeign 'uconnect :arguments '(string integer) :pass-types '(:by-value :by-value))
  (ff:defforeign 'uread :arguments '(integer integer string) :return-type :integer :pass-types '(:by-value :by-value :by-value))
  (ff:defforeign 'ureadwait :arguments '(integer integer string) :return-type :integer :pass-types '(:by-value :by-value :by-value))
  (ff:defforeign 'uwrite :arguments '(integer string) :pass-types '(:by-value :by-value))
  (ff:defforeign 'ugetpeername :arguments '(integer) :pass-types '(:by-value) :return-type :integer) 
  (ff:defforeign 'uwritefile :arguments '(integer string))
  )

;; ---------------------------------------------------------------------------
;; Section 1 : The socket database. 
;; ---------------------------------------------------------------------------


(defvar socket*sockets 

  ;;; Edited  : 02.12.1998
  ;;; Authors : serge
  ;;; Descri. : a property list of symbolic socket names and sockets.
    
  #+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))  nil
  #+(or cmu (and allegro-version>= (version>= 5 0))) (make-hash-table) "A hashtable of symbolic socket names and sockets."
  )

(defmacro socket=sockets ()

  ;;; Edited  : 02.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : /
  ;;; Value   : the actual value of `socket*sockets
  
  `socket*sockets)


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-reset ()

  ;;; Edited  : 03.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : resets all sockets. It closes any connected socket and removes the entries from the
  ;;;           socket database. 
  ;;; Value   : undef.

  (setq socket*end-of-string (code-char 128))
  (mapcf #'(lambda (socketname socket)
	     (when (not (equal socket 'unconnected))
	       (socket-close socketname)))
	 (socket=sockets))
  (setq socket*sockets nil))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-reset ()
  (maphash #'(lambda (socketname socket)
	    (socket-close socketname))
	 (socket=sockets))
  (clrhash (socket=sockets)))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket=socketname.socket (socketname)

  ;;; Edited  : 19. Feb 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (getf (getf (socket=sockets) socketname) 'socket))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket=socketname.socket (socketname)

  ;;; Edited  : 19. Feb 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (getf (gethash socketname (socket=sockets)) 'socket))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket=socketname.buffer-string (socketname)

  ;;; Edited  : 19. Feb 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (getf (getf (socket=sockets) socketname) 'buffer-string))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket=socketname.buffer-string (socketname)

  ;;; Edited  : 19. Feb 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (getf (gethash socketname (socket=sockets)) 'buffer-string))



#+(and (not cmu) (and allegro-version>= (version>= 5 0)))
(defun socket-find.socket (socketname)
  (socket=socketname.socket socketname))


;; ---------------------------------------------------------------------------
;; Section 2 : Defining/Undefining sockets.
;; ---------------------------------------------------------------------------


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-define (socketname)

  ;;; Edited  : 02.12.1998
  ;;; Authors : serge
  ;;; Input   : an sexpr which is the symbolic name of a socket. 
  ;;; Effect  : creates an entry for this socket.
  ;;; Value   : undef. 
  
  (cond ((and socketname (null (getf socket*sockets socketname)))
	 (setf (getf (getf socket*sockets socketname) 'port) nil)
	 (setf (getf (getf socket*sockets socketname) 'socket) 'unconnected)
	 (setf (getf (getf socket*sockets socketname) 'buffer-string) "")
	 )
	((null socketname) 
	 (format t "Invalid socket name ~A!" socketname))
	(T (print "Redefinition of existing socket-names is not allowed!")
	   nil)))
	 

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-define (socketname)

  ;;; Edited  : 20-SEP-2000
  ;;; Authors : pollet       
  ;;; Input   : An sexpr which is the symbolic name of a socket.
  ;;; Effect  : Creates an entry for this socket.
  ;;; Value   : T for success, NIL for failure.

  (cond ((and socketname (null (gethash socketname socket*sockets)))
	 (setf (getf (gethash socketname socket*sockets) 'socket) 'unconnected)
	 (setf (getf (gethash socketname socket*sockets) 'buffer-string) "")
	 T)
	((null socketname)
	 (format t "Invalid socket name ~A!" socketname)
	 nil)
	(T 
	 (print "Redefinition of existing socket-names is not allowed!")
	 nil)))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-delete (socketname)
  
  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : an sexpr defining a socket, i.e. a socketname
  ;;; Effect  : deletes the socketname
  ;;; Value   : /

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (equal (getf socket 'socket) 'unconnected))
	   (remf socket*sockets socketname))
	  ((null socket) (format t "Unknown socket ~A given to socket-undefine!" socketname) "")
	  (t (format t "Socket ~A is still connected in socket-delete!" socketname) ""))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-delete (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : An sexpr defining a socket, i.e. a socketname.
  ;;; Effect  : Deletes the socketname.
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (equal socket 'unconnected))
	   (remhash socketname socket*sockets)
	   T)
	  ((null  socket)
	   (format t "Unknown socket ~A given to socket-undefine!"
		   socketname)
	   nil
	   )
	  (t (format t "Socket ~A is still connected in socket-delete!" socketname)
	     nil
	     ))))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-active? (socketname)

  ;;; Edited  : 11. Feb 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname
  ;;; Effect  : checks whether a socket of this name exists and, if so, if it is connected.
  ;;; Value   : T, if the check succeeds; NIL otherwise.
  
  (let ((socket (getf (socket=sockets) socketname))
	(astring (make-string 1))
	val)
    (if (getf socket 'port) t
      (cond ((and socket (numberp (getf socket 'socket)) T)
	     (setq val (uread (getf socket 'socket) 1 astring))
	     (cond ((eq val -2) ;; Socket is there, but nothing on the socket.
		    T)
		   ((eq val 1) ;; Socket is there and read one character.
		    ;; Saving the character in the buffer-string of the socket.
		    (setf (getf (getf (socket=sockets) socketname) 'buffer-string) 
			  (concatenate 'string 
				       (getf (getf (socket=sockets) socketname) 'buffer-string) (copy-seq (string (elt  astring 0)))))
		    T)))))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-active? (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A socketname.
  ;;; Effect  : Checks whether a socket of this name exists and, if so, if it is connected.
  ;;; Value   : T, if the check succeeds; NIL otherwise.

  (let ((socket (socket=socketname.socket socketname)))
    (and socket
	 (or (and (streamp socket) (open-stream-p socket)
		  (not (and (listen socket)
			    (eq (peek-char nil socket nil 'eof) 'eof))))  ;active sockets
	     (not (equal socket 'unconnected))))))                       ;passive sockets

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-active? (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A socketname.
  ;;; Effect  : Checks whether a socket of this name exists and, if so, if it is connected.
  ;;; Value   : T, if the check succeeds; NIL otherwise.

  (let ((socket (socket=socketname.socket socketname)))
    (and socket
	 (streamp socket)
	 (open-stream-p socket)
	 )))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-receives? (socketname)

  ;;; Edited  : 10. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname 
  ;;; Effect  : checks whether there is something coming over the socket.
  ;;; Value   : T, if there is something; NIL otherwise.

  (let ((socketentry (getf (socket=sockets) socketname))
	(astring (make-string 1))
	val)
    (cond ((and socketentry (numberp (getf socketentry 'socket)) T)
	   (setq val (uread (getf socketentry 'socket) 1 astring))
	   (cond ((eq val -2) ;; Socket is there, but nothing on the socket.
		  nil)
		 ((eq val 1) ;; Socket is there and read one character.
		  ;;; Saving the character in the buffer-string of the socket.
		  (setf (getf (getf (socket=sockets) socketname) 'buffer-string) 
		    (concatenate 'string 
		      (getf (getf (socket=sockets) socketname) 'buffer-string) (copy-seq (string (elt  astring 0)))))
		  T))))))
  
#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-receives? (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet serge       
  ;;; Input   : A socketname.
  ;;; Effect  : Checks whether there is something coming over the socket.
  ;;; Value   : T, if there is something; NIL otherwise.

  (let ((socket (socket=socketname.socket socketname)))
    (and socket (socket-active? socketname) 
	 #+(and allegro-version>= (version>= 5 0)) (stream::stream-listen socket)
	 #+CMU (common-lisp:listen socket)
	 )))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-fd (socketname)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : a socketname
  ;;; Effect  : /
  ;;; Value   :the file descriptor of this socket

  (getf (getf (socket=sockets) socketname) 'socket)
  )

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-fd (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A socketname.
  ;;; Effect  : -
  ;;; Value   : The file descriptor of this socket.

  (let ((socket (socket=socketname.socket socketname)))
    (when (socket-active? socketname) 
      #+(and allegro-version>= (version>= 5 0)) (socket::socket-os-fd socket)
      #+cmu (SYSTEM:FD-STREAM-FD socket)
      )))

;; ---------------------------------------------------------------------------
;; Section 3 : Connecting/Closing sockets.
;; ---------------------------------------------------------------------------


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-connect (host port socketname)

  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A host and a portnumber to connect to.
  ;;; Effect  : Connects to the specified socket.
  ;;; Value   : /

  (let (socket)
    (cond ((and (setq socket (getf (socket=sockets) socketname)) 
		(eq (getf socket 'socket) 'unconnected))
	   (setf (getf (getf socket*sockets socketname) 'socket) (uconnect host port)))
	  ((null socket)
	   (format t "Socket name ~A not defined" socketname))
	  (t (format t "Socket ~A already connected." socketname)))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-connect (host port socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A host and a portnumber to connect to.
  ;;; Effect  : Connects to the specified socket.
  ;;; Value   : T for success, NIL for failure.

  (let (socket)
    (cond ((and (setq socket (socket=socketname.socket socketname))
		(equal socket  'unconnected))
	   (setf (gethash socketname socket*sockets)
		 (list 'socket 
		       #+(and allegro-version>= (version>= 5 0)) 
		       (socket::make-socket :remote-host host
					    :remote-port port
					    :type :stream
					    :address-family :internet
					    :connect :active)
		       #+CMU 
		       (system:make-fd-stream (connect-to-inet-socket host port)
					      :input t :output t)
		       'buffer-string ""))
	   T)
	  ((null socket)
	   (format t "Socket name ~A not defined" socketname)
	   nil)
	  (t (format t "Socket ~A already connected." socketname)
	     nil))))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-close (socketname)
  
  ;;; Edited  : 17.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : /
  ;;; Effect  : Closes the connection to the socket.
  ;;; Value   : /

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (when (> (uclose (getf socket 'socket)) -1)
	     (setf (getf socket 'port) nil)
	     (setf (getf socket 'socket) 'unconnected))
	   (setf (getf socket 'buffer-string) "")
	   )
	  ((null  socket) (format t "Unknown socket ~A given to socket-close!" socketname) "")
	  (t (format t "Socket ~A is not connected in socket-close!" socketname) ""))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-close (socketname)

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A socket name.
  ;;; Effect  : Closes the connection to the socket. If error-p is not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (not (equal socket 'unconnected)))
	   #+(and allegro-version>= (version>= 5 0)) (socket::close socket)
	   #+CMU                                     (close-socket (SYSTEM:FD-STREAM-FD socket))
	   (setf (getf (gethash socketname (socket=sockets)) 'socket) 'unconnected)
	   T)
	  ((null  socket)
	   (format t "Unknown socket ~A given to socket-close!" socketname)
	   nil
	   )
	  (t (format t "Socket ~A is not connected in socket-close!" socketname)
	     nil
	     ))))

;; ---------------------------------------------------------------------------
;; Section 4 : Read from/Write to sockets.
;; ---------------------------------------------------------------------------


(defvar socket*end-of-string 

  ;;; Edited  : 09.12.1998
  ;;; Authors : serge
  ;;; Descri. : the character indicating the end of a string to be read from a socket. 

  (code-char 128))


(proclaim '(type character socket*end-of-string))

(defmacro socket=end-of-string ()

  ;;; Edited  : 09.12.1998
  ;;; Authors : serge
  ;;; Input   : /
  ;;; Effect  : /
  ;;; Value   : the actual value of `socket*end-of-string

  `socket*end-of-string)


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket=readloop (socket &optional wait? (eos (socket=end-of-string)))

  ;;; Edited  : 17.03.1997 01.12.1998 09.12.1998 12. Jan 1999 19. Feb 1999
  ;;; Authors : hess       serge      serge      serge        serge       
  ;;; Input   : a socket description, which is a property list ('SOCKET socket 'BUFFER-STRING string)
  ;;; Effect  : /
  ;;; Value   : A string read from the socket, if there is some terminating with (socket=end-of-string). 
  ;;;           NIL if there is nothing on the socket. If there is something on the socket, but the end-of-string
  ;;;           character (socket=end-of-string) has not been read, waits for this character. 
  
  (let ((val 1)
	(astring (make-string 1 :initial-element (code-char 32)))
	(local-stream (make-string-output-stream))
	(overall-string (getf socket 'buffer-string))
	(the-socket (getf socket 'socket))
	(result nil)
	tmp)
    ;; Writing the actual content of the socket-buffer into the local-stream
    (write-string (socket=socketname.buffer-string socketname) local-stream)
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (eq eos (elt astring 0)) (< val 1)))
      (when (eq 1 (setq val (if wait? (ureadwait the-socket 1 astring) 
			      (uread the-socket 1 astring))))
	;;; Store the recently read character in the overall string.
	(unless (eq eos (elt astring 0))
	  (when (and tmp (eq tmp #\\)
		     (not (member (elt astring 0) (list #\\ #\"))))
	    (write-char #\\ local-stream))
	  (write-char (elt astring 0) local-stream)
	  (setq tmp (elt  astring 0))))
      ;; (setq overall-string (concatenate 'string overall-string (copy-seq (string (elt astring 0))))))
      )
    (cond ((< val 1) ;;; if we got an error on the socket, save the string read so far in the socket buffer and return NIL.
	   (setq result nil)
	   (setf (getf socket 'buffer-string) (get-output-stream-string local-stream))
	   (cond ;; ((eq val -2) ;; (format t "No more symbols on the socket.~%"))
		 ((eq val -1) (format t "General read error on the socket.~%"))
		 ((eq val 0) (format t "Got an EOF on the socket. The other side might have closed/lost the socket.~%")))
	   )
	  (T ;;; otherwise return the actual string and delete the socket buffer.
	   (setf (getf socket 'buffer-string) "")
	   (setq result (get-output-stream-string local-stream))))
    result))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket=readloop (socketname &optional wait? (eos (socket=end-of-string)))

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A socket.
  ;;; Effect  : -
  ;;; Value   : A string read from the socket, if there is some terminating with (socket=end-of-string).
  ;;;           NIL if there is nothing on the socket. If there is something on the socket, but the end-of-string
  ;;;           character (socket=end-of-string) has not been read, waits for this character.

  (let 	((astring nil)
	 (socket (socket=socketname.socket socketname))
	 (local-stream (make-string-output-stream))
	 (result nil)
	 (tmp nil))
    ;; Writing the actual content of the socket-buffer into the local-stream
    (write-string (socket=socketname.buffer-string socketname) local-stream)
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (eq eos astring)
	     (eq astring :eof)
	     )
	 )
      (when (socket-receives? socketname) 
	(setf astring 
	      #+ALLEGRO (if wait? (stream-read-char socket) (read-char-no-hang socket))
	      #+CMU18   (if wait? (common-lisp:read-char socket) (common-lisp:read-char-no-hang socket))
	      )
	;; (format t "Read on socket ~A the character: ~S~%" socketname astring)
	;; Store the recently read character in the overall string.
	(unless (or (eq eos astring) (eq astring :eof))
	  (when (and tmp (eq tmp #\\)
		     (not (member astring (list #\\ #\"))))
	    (write-char #\\ local-stream))
	  (write-char astring local-stream)
	  (setq tmp astring)))
      )
    (cond ((and astring (eq astring eos))
	   (setq result (get-output-stream-string local-stream))
	   (setf (getf (gethash socketname (socket=sockets)) 'buffer-string) ""))
	  ((socket-active? socketname)
	   (setf (getf (gethash socketname (socket=sockets)) 'buffer-string) 
		 (get-output-stream-string local-stream)))
	  (t (format t "General read error on the socket. The other side might have loast/closed the connection.~%")))
    result))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-read (socketname &optional (wait? nil) (eos (socket=end-of-string)))

  ;;; Edited  : 24.03.1997 01.12.1998  19. Feb 1999
  ;;; Authors : hess       serge       serge       
  ;;; Input   : /
  ;;; Effect  : Write handshake signal to socket.
  ;;; Value   : The string read from the socket.
  
  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (socket=readloop socket wait? eos))
	  ((null socket) (format t "Unknown socket ~A given to read from!" socketname) "")
	  (t (format t "Socket ~A is not connected in read-socket!" socketname) ""))))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-read (&optional (socketname :inout) (wait? nil) (eos (socket=end-of-string)))

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : Socketname, wait-switch, signal-error-switch.
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : The string read from the socket on success, the empty string on failure.

  (let* ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (not (equal socket 'unconnected))) 
	   (socket=readloop socketname wait? eos))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t
	   (format t "Socket ~A is not connected in read-socket!" socketname)
	   ""))))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-write (string socketname &optional (eos (socket=end-of-string)))
  
  ;;; Edited  : 24.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A string and the name of a socket.
  ;;; Effect  : Wait for ready signal, then write string to socket.
  ;;; Value   : undef. 

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (uwrite (getf socket 'socket) (concatenate 'string string (string eos))))
	  ((null socket) (format t "Unknown socket ~A given to write to!" socketname) nil)
	  (t (format t "Socket ~A is not connected in write-socket!" socketname) nil))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-write (string &optional (socketname :inout)
			    (eos (string socket*end-of-string))
			    )

  ;;; Edited  : 20-SEP-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A string and the name of a socket.
  ;;; Effect  : Wait for ready signal, then write string to socket..
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (write-string (concatenate 'string string (string eos)) socket)
	   (force-output socket)
	   T)
	  ((null socket)
	   (format t "Unknown socket ~A given to write to!" socketname)
	   nil
	   )
	  (t
	   (format t "Socket ~A is not connected in write-socket!" socketname)
	   nil
	   ))))


;; ---------------------------------------------------------------------------
;; Section 5 : Stuff for TCP-Server and HTTP
;; ---------------------------------------------------------------------------

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-bind (port socketname)

  ;;; Edited  : 04. Aug 2000  06. Sep 2000
  ;;; Authors : Pollet        serge       
  ;;; Input   : A port and a the socketname of a defined socket.
  ;;; Effect  : Establish a server at port PORT. If error-p in not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : T for success, NIL for failure.
  
  (let ((socket (getf (socket=sockets) socketname)))
    (if socket
	(if (eq (getf socket 'socket) 'unconnected)
	    (let ((fd (ubind port)))
	      (if (and (numberp fd) (plusp fd))
		  (progn
		    (setf (getf socket 'socket) fd)
		    (setf (getf socket 'port) port)
		    t)
		(progn (format t "Problems to bind port ~A to socket ~A." port socketname)
		       nil)))
	  (progn (format t "Socket ~A already connected." socketname)
		 nil))
      (progn (format t "Socket name ~A not defined" socketname)
	     nil))))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-bind (port socketname)

  ;;; Edited  : 04-AUG-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A port and a the socketname of a defined socket.
  ;;; Effect  : Establish a server at port PORT. If error-p in not NIL an error is
  ;;;           signaled on failure.
  ;;; Value   : T for success, NIL for failure.

  (let ((socket (socket=socketname.socket socketname))
	(newsocket nil))
    (if socket
	(if (eq socket 'unconnected)
	    #+(and allegro-version>= (version>= 5 0))
	    (handler-case 
	     (progn (setf (getf (gethash socketname (socket=sockets)) 'socket)  
			  (socket::make-socket :connect :passive :local-port port))
		    t)
	     (excl::socket-error (x) 
				 (format t "Error ~%~A~% while binding socket ~A on port ~D!~%" x socketname port)
				 nil))
	    #+CMU 
	    (handler-case 
	     (progn (setf (getf (gethash socketname (socket=sockets)) 'socket)
			  (system:make-fd-stream (create-inet-listener port)
						 :input t :output t))
		    T)
	     (error (x) 
		    (format t "Error ~%~A~% while binding socket ~A on port ~D!~%" x socketname port)
		    nil))
	  (progn (format t "Socket ~A already connected." socketname)
		 nil))
      (progn
	(format t "Socket name ~A not defined" socketname)
	nil))))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-accept (serversocket connectsocket)

  ;;; Edited  : 04. Aug 2000  06. Sep 2000
  ;;; Authors : Pollet        serge       
  ;;; Input   : Two socketnames
  ;;; Effect  : Waits for a connection on SERVERSOCKET. If this
  ;;;           happens, CONNECTSOCKET will be connected to client.
  ;;; Value   : T for success, NIL for failure.

  (let ((ssock (getf (socket=sockets) serversocket))
	(csock (getf (socket=sockets) connectsocket)))
    (if (and csock ssock)
	(if (eq (getf csock 'socket) 'unconnected)
	    (let ((fd (uaccept (getf ssock 'socket))))
	      (if (and (numberp fd) (plusp fd))
		  (progn (setf (getf csock 'socket) fd)
			 t)
		(progn (format t "Problems to accept ~A." serversocket)
		       nil)))
	  (progn (format t "Socket ~A already connected." connectsocket)
		 nil))
      (progn (format t "Socket name ~A or ~A not defined" serversocket connectsocket)
	     nil))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-accept (serversocket connectsocket)

  ;;; Edited  : 04-AUG-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : Two socketnames.
  ;;; Effect  : Waits for a connection on SERVERSOCKET. If this happens, CONNECTSOCKET will be connected to client.
  ;;;           If error-p is not NIL an error is signalled on failure.
  ;;; Value   : T for success, NIL for failure.

  (let ((ssock (socket=socketname.socket serversocket))
	(csock (socket=socketname.socket connectsocket)))
    (cond ((not (and csock ssock))
	   (format t "Socket name ~A or ~A not defined" serversocket connectsocket)
	   nil)
	  ((not (eq csock 'unconnected))
	   (format t "Socket ~A already connected." connectsocket)
	   nil)
	  ((eq ssock 'unconnected)
	   (format t "Socket ~A not connected." connectsocket)
	   nil)
	  ((streamp csock)
	   (format t "Socket ~A is not a passive socket." connectsocket)
	   nil)
	  (T
	   (progn
	     (setf (getf (gethash connectsocket (socket=sockets)) 'socket)
		   #+(and allegro-version>= (version>= 5 0)) (socket::accept-connection ssock)
		   #+CMU (system:make-fd-stream (accept-tcp-connection (system:fd-stream-fd ssock))
						:input t :output t)
		   )
	     T)))))

#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket=readuntil (socket what &optional wait?)

  ;;; Edited  : 06. Aug 2000 06. Sep 2000
  ;;; Authors : pollet       serge       
  ;;; Input   : a socket description, which is a property list ('SOCKET socket 'BUFFER-STRING string)
  ;;;           and a NUMBER or CHAR.
  ;;; Effect  : Reads NUMBER chars from the socket or untill CHAR or (socket=end-of-string)
  ;;; Value   : Returns a string containing the chars read.
  
  (let* ((val 1)
	 (astring (make-string 1 :initial-element (code-char 32)))
	 (local-stream (make-string-output-stream))
	 (overall-string (getf socket 'buffer-string))
	 (the-socket (getf socket 'socket))
	 (result nil)
	 (counter 0)
	 (test (etypecase what
		 (number #'(lambda (x)(declare (ignore x))(= counter what)))
		 (character #'(lambda (x)(eq what (elt x 0))))
		 (null #'(lambda (x)(declare (ignore x)) nil)))))
		
    (do ()
	;;; Read from socket one character, until we got the end-of-string character or an error. 
	((or (funcall test astring)
	     (eq (socket=end-of-string) (elt astring 0))
	     (= val 0)
	     ))
      (when (eq 1 (setq val (if wait? (ureadwait the-socket 1 astring) 
			      (uread the-socket 1 astring))))
	;; Store the recently read character in the overall string.
	;; (if not eq to socket=end-of-string
	(unless (or (eq (socket=end-of-string) (elt astring 0)))
	  (setq counter (1+ counter))
	  (write-char (elt  astring 0) local-stream)))
      ;; (setq overall-string (concatenate 'string overall-string (copy-seq (string (elt astring 0))))))
      )
    (cond ((and (< val 1)(not (= val -2))) ;;; if we got an error on the socket, save the string read so far in the socket buffer and return NIL.
	   (setq result nil)
	   (setf (getf socket 'buffer-string) 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))
	   (cond ;; ((eq val -2) ;; (format t "No more symbols on the socket.~%"))
	         ((eq val -1) (format t "General read error on the socket.~%"))
		 ((eq val 0) (format t "Got an EOF on the socket. The other side might have closed/lost the socket.~%")))
	   )
	  (T ;;; otherwise return the actual string and delete the socket buffer.
	   (setf (getf socket 'buffer-string) "")
	   (setq result 
	     (concatenate 'string overall-string (get-output-stream-string local-stream)))))
    result))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-read.line (socketname &optional (wait? nil))

  ;;; Edited  : 06. Aug 2000 06. Sep 2000
  ;;; Authors : pollet       serge       
  ;;; Input   : Socketname.
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is signaled on failure.
  ;;; Value   : The string containing the line read from the socket.

  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected))) 
	   (socket=readuntil socket #\newline wait?))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-read.line (socketname &optional (wait? nil))

  ;;; Edited  : 06-AUG-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : Socketname.
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is signaled on failure.
  ;;; Value   : The string containing the line read from the socket.

  (let* ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (read-line socket nil nil))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-read.content (size socketname &optional (wait? nil) (error-p nil))

  ;;; Edited  : 06. Aug 2000 06. Sep 2000
  ;;; Authors : pollet       serge       
  ;;; Input   : Socketname and a number.
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is signaled on failure.
  ;;; Value   : The string containing SIZE chars read from the socket.
  
  (let* ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected))) 
	   (socket=readuntil socket size wait?))
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))


#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-read.content (size socketname &optional (wait? nil))

  ;;; Edited  : 06-AUG-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : Socketname and a number.
  ;;; Effect  : Write handshake signal to socket. If error-p is not NIL an error is signaled on failure.
  ;;; Value   : The string containing SIZE chars read from the socket.

  ;; (warn "Socket-read.content with size ~A" size)
  (let* ((socket (socket=socketname.socket socketname)))
    (cond ((and socket (not (equal socket 'unconnected)))
	   (let ((input (make-string size)))
	     (read-sequence input socket)
	     input))	  
	  ((null socket)
	   (format t "Unknown socket ~A given to read from!" socketname)
	   "")
	  (t (format t "Socket ~A is not connected in read-socket!" socketname)
	     ""))))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-write.file (header pathname &optional (socketname :inout)
				 (eos (string socket*end-of-string))
				 (error-p nil))
  
  ;;; Edited  : 24.03.1997 01.12.1998
  ;;; Authors : hess       serge
  ;;; Input   : A string and the name of a socket.
  ;;; Effect  : Wait for ready signal, then write string to socket. If error-p is not NIL
  ;;;           an error is signaled on failure.
  ;;; Value   : T for success, NIL for failure. 

  (let ((socket (getf (socket=sockets) socketname)))
    (cond ((and socket (not (equal (getf socket 'socket) 'unconnected)))
	   (uwrite (getf socket 'socket) header)
	   (uwritefile (getf socket 'socket) pathname)
	   T)
	  ((null socket)
	   (when error-p
	     (error "Unknown socket ~A given to write to!" socketname))
	   (format t "Unknown socket ~A given to write to!" socketname)
	   nil
	   )
	  (t
	   (when error-p
	     (error "Unknown socket ~A given to write to!" socketname))
	   (format t "Socket ~A is not connected in write-socket!" socketname)
	   nil
	   ))))


#+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
(defun socket-get.peername (socketname)

  ;;; Edited  : 29. Aug 2000 06. Sep 2000
  ;;; Authors : pollet       serge       
  ;;; Input   : A Socketname
  ;;; Effect  : Calls the foreign functions 'ugetpeername'.
  ;;; Value   : A string with the IP number of the connected host.
  
  (let ((socket (getf (socket=sockets) socketname))
	val)
    (when (and socket (numberp (getf socket 'socket))
	       (setq val (ugetpeername (getf socket 'socket))))
      (unless (= val -1)
	#+(or allegro-v5.0 allegro-v5.0.1)(excl:native-to-string val)
	#+(or allegro-v4.3 allegro-v4.3.1)(ff:char*-to-string val)
	))))

#+(or cmu (and allegro-version>= (version>= 5 0)))
(defun socket-get.peername (socketname)

  ;;; Edited  : 29-AUG-2000 14. Feb 2001
  ;;; Authors : Pollet      serge       
  ;;; Input   : A Socketname
  ;;; Effect  : -
  ;;; Value   : A string with the IP number of the connected host.

  (let ((socket (socket=socketname.socket socketname)))
    (when socket
      #+(and allegro-version>= (version>= 5 0)) (socket::ipaddr-to-dotted (socket::remote-host socket))
      #+CMU  (multiple-value-bind (ipaddr port) (get-peer-host-and-port (system:fd-stream-fd socket))
	       ipaddr)
      )))


(provide "socket-interface")

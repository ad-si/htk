(in-package :inka)


(defun dummy-listen (socketname)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (loop 
    (pro-process.socket.wait.for.input.available socketname
						 (format nil "Waiting for input on socket ~A" socketname))
    (format t "~A : ~A~%" socketname (socket-read socketname))))


;;; -----------------------------------------------------------------------------
;;; Section 1: General server/client functionalities
;;; -----------------------------------------------------------------------------

(defvar in*active.servers

  ;;; A property list indexed by servernames and containing closures to execute when shutting
  ;;; the server.

  nil)

(defun in-start.server (servername port listenerfunction &key (new-thread? t))

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : a symbol for the servername, its port and the listenerfunction handling input from
  ;;;           clients
  ;;; Effect  : creates a new socket of this name
  ;;; Value   : T, if the server was successfully installed; NIL otherwise. 

  ;; reseting ...
  (in-stop.server servername)
  ;; initializing
  (socket-define servername)
  (format t "Starting server ~A on port ~A...~%" servername port)
  (let ((success (socket-bind port servername)))
    (when success
      (if new-thread? 
	  (setf (getf in*active.servers servername) (in=server.create.listener servername listenerfunction))
	(progn 
	  (setf (getf in*active.servers servername) (pro-process.actual.process))
	  (in=actual.process.create.server servername listenerfunction))
	))
    success))


(defun in-stop.server (servername)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (when (socket-active? servername)
    (when (socket-close servername)
      (socket-delete servername))
    )
  (when (getf in*active.servers servername)
    (in=server.stop.client.listeners servername)
    (pro-process.kill (getf in*active.servers servername)))
  (remf in*active.servers servername))


(defun in=server.create.listener (servername listenerfunction)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (pro-process.create :name (string servername)
		      :function #'in=server.listener
		      :args (servername listenerfunction)))


(defun in=actual.process.create.server (servername listenerfunction)

  ;;; Edited  : 07. Oct 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (in=server.listener servername listenerfunction))


(defun in=server.listener (servername listenerfunction)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (while (socket-active? servername)
    (let ((clientname (gentemp (concatenate 'string (string servername) "CLIENT"))))
      (pro-process.socket.wait.for.input.available servername
						   (format nil "Server ~A : Waiting for connect request." servername))
      (socket-define clientname)
      (socket-accept servername clientname)
      (setf (getf (getf (pro-process.property.list (pro-process.actual.process)) :running-listeners) clientname)
	    (in=listener.create clientname listenerfunction (pro-process.actual.process)))))
  (in=server.stop.client.listeners servername))
  

(defun in=server.stop.client.listeners (servername)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (when (getf in*active.servers servername)
    (mapcf #'(lambda (clientname listenerprocess)
	       (pro-process.kill listenerprocess)
	       (socket-close clientname)
	       (socket-delete clientname))
	   (getf (pro-process.property.list (getf in*active.servers servername)) :running-listeners))))


(defun in=listener.create (socketname listenerfunction server-process)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (pro-process.create :name (string socketname)
		      :function #'(lambda (socketname)
				    (prog1
					(funcall listenerfunction socketname)
				      (socket-close socketname)
				      (socket-delete socketname)
				      ;; removing the process info in the server process after the listener terminated
				      (remf (getf (pro-process.property.list server-process) :running-listeners) socketname)))
		      :args (socketname)))


;;; -----------------------------------------------------------------------------
;;; Section 3 : HTTP Server
;;; -----------------------------------------------------------------------------

(defparameter in*http.server.port 40000)

(defparameter in*http.server.name :http)

(defun in-http.server.start ()

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (in-start.server in*http.server.name in*http.server.port #'in=http.client.listener))


(defun in-http.server.stop ()

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (in-stop.server in*http.server.name))


(defun in-http.server.restart ()

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (in-http.server.stop)
  (in-http.server.start))
  


(defun in=http.client.listener (socketname)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  (let ((header (in=http.read.header socketname)))
    header))


(defun in=http.read.header (socketname)

  ;;; Edited  : 06. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((content '())
	(successivenewlines 0)
	(readline ""))
    (while (and (socket-receives? socketname)
		(< successivenewlines 2))
      (pro-process.socket.wait.for.input.available socketname
						   (format nil "Waiting for input on socket ~A" socketname))
      (setq readline (socket-read socketname nil #\Newline))
      (when (stringp readline)
	(setq readline (string-trim '(#\return) readline))
	(format t "~A: ~S~%" socketname readline)
	(if (string= readline "")
	    (incf successivenewlines)
	  (setq successivenewlines (max 0 (1- successivenewlines))))
	(push readline content)))
    (reverse content)))
      



;;; -*- Syntax: Common-lisp; Package: INKA; Mode: LISP; Base: 10 -*-
;;; $Header$

;;; 06.01.93 de ---                         add comments 
(in-package :inka)

;;;;;    **************************************************************
;;;;;    *                                                            *
;;;;;    * The PROCESS-Handling of the INKA-System.                   *
;;;;;    * ========================================                   *
;;;;;    *                                                            *
;;;;;    **************************************************************
;;;;;
;;;;;    Note that all functions in this module are extensions to Common
;;;;;    Lisp for Lucid Common LISP
;;;;;
;;;;;    The Module is mainly a wrapper for some multiprocessing functions
;;;;;    (Thanx to the INKA people and especially (serge@dfki.de)
;;;;;
;;;;;    Nice extensions also by (sorge@ags.uni-sb.de), who integrated
;;;;;    this file (with a different naming conventions) into the
;;;;;    OMEGA system. 
;;;;;
;;;;;    **************************************************************


(defun pro-reset ()

  ;;; Edited  : 24. Feb 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (setq pro*debug nil)
  (setq pro*counter 0))


(defparameter pro*debug 

  ;;; Edited  : 24. Feb 2000
  ;;; Authors : serge       
  ;;; Descri. : 

  nil)

(defparameter pro*counter

  ;;; Edited  : 24. Feb 2000
  ;;; Authors : serge       
  ;;; Descri. : 

  0)

(defun pro-all.processes ()

  ;;; Edited  : 28. Feb 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+ALLEGRO mp:*all-processes*
  #+LUCID   lcl:*all-processes*
  #+CMU     (MULTIPROCESSING:all-processes)
  )


(defun pro-process.actual.process ()

  #+ALLEGRO        mp:*current-process* 
  #+LUCID          lcl:*current-process*
  #+CMU            (MULTIPROCESSING:CURRENT-PROCESS)
  )

(defmacro pro-without.scheduling (&body body)

  #+ALLEGRO     `(excl:without-interrupts ,@body)
  #+LUCID       `(with-scheduling-inhibited ,@body)
  #+CMU         `(multiprocessing:without-scheduling (progn . ,body))
  )

(defmacro pro-process.allow.schedule (&optional process)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   :

  #+ALLEGRO `(mp:process-allow-schedule ,process)
  #+LUCID   `(process-allow-schedule ,process)
  )


(defmacro pro-quit ()

  #+ALLEGRO     `(exit)
  #+LUCID       `(quit)
  #+CMU         `(quit)
  )


(defmacro pro-start-scheduler ()

  #+ALLEGRO      `(mp:start-scheduler)
  #+LUCID        nil
  #+CMU           `(multiprocessing::startup-idle-and-top-level-loops)
  )


(defmacro pro-process.create (&key name (priority 100) function args wait-function wait-args)

  ;;;  Input:  name:          name of process
  ;;;          function:      the initial function which starts the process
  ;;;          args:          list of arguments to the initial function
  ;;;          wait-function: the wait function is used by the scheduler to test whether
  ;;;                         the process should run
  ;;;          wait-args:     list of arguments to the wait function
  ;;;  Effect: A process is created.
  ;;;  Value:  A process

  #+ALLEGRO `(mp:process-run-function (list :name ,name :priority ,priority) ,function ,@args)
  #+LUCID `(append
	    (list 'make-process
		  :name ,name :function ,function :args ,args
		  :priority ,priority)
	    (cond ((neq ,wait-function nil) (list :wait-function ,wait-function)))
	    (cond ((neq ,wait-args nil) (list :wait-args ,wait-args))))
  #+(and :CMU (not :cmu18))
  `(append (list 'multiprocessing:make-process ,function :name ,name)
	   (cond ((neq ,wait-function nil) (list :wait-function ,wait-function)))
	   (cond ((neq ,wait-args nil) (list :wait-args ,wait-args))))
  #+CMU18 `(let ((process (multiprocessing:make-process (lambda () (funcall ,function ,@args))
							:name ,name)))
	     (setf (multiprocessing::process-wait-function process) ,wait-function)
	     (setf (multiprocessing::process-wait-function-args process) ,wait-args)
	     process)
  )


(defun pro-process.is.active (process)

  ;;;  Input:  A process
  ;;;  Effect: Test the state of the process.
  ;;;          A process is active, if it is either running or waiting to run.
  ;;;          A process is inactive, if it is a process that is alive but that cannot be run.
  ;;;          A process is dead, if it has been killed or if it has run to completion.
  ;;;  Value:  True, if the process is activ, otherwise Nil.
  
  #+ALLEGRO  (if (mp:process-active-p process) t nil)
  #+LUCID    (if (lcl:process-active-p process) t nil)
  #+(and :CMU (not :cmu18))  (eq (multiprocessing:process-state process) :active)
  #+cmu18    (if (multiprocessing:process-active-p process) t nil)
  )


(defun pro-process.is.running (process)

  ;;; Edited  : 28. Feb 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+ALLEGRO (if (mp:process-runnable-p process) t nil)
  #+LUCID (if (lcl:process-runnable-p process) t nil)
  #+(and :CMU (not :cmu18)) (eq (multiprocessing:process-state process) :active)
  #+cmu18  (if (multiprocessing:process-active-p process) t nil)
)


(defmacro pro-process.wait (whostate function &rest args)

  ;;;  Input:  whostate: specifies a string that will be displayed as the state of the process
  ;;;          function: the wait function is used by the scheduler to test whether the process should run
  ;;;          args:     list of arguments to the wait function
  ;;;  Effect: Suspends the process until function returns a non-nil value when applied to args.
  ;;;  Value:  The non-nil value
  
  #+ALLEGRO `(mp:process-wait ,whostate ,function ,@args)
  #+LUCID   `(lcl:process-wait ,whostate ,function ,@args)
  #+CMU     `(multiprocessing:process-wait ,whostate ,function ,@args)
  )

(defmacro pro-process.socket.wait.for.input.available (socketname whostate)

  ;;; Edited  : 12. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
            `(mp:wait-for-input-available (socket-fd ,socketname) :whostate ,whostate)
  #+(and allegro-version>= (version>= 5 0))
            `(mp:wait-for-input-available (socket-find.socket ,socketname) :whostate ,whostate)
  #+LUCID   `(lcl:process-wait ,whostate #'(lambda () (socket-receives? ,socketname)))
  #+CMU     `(MULTIPROCESSING:PROCESS-WAIT ,whostate #'(lambda () (socket-receives? ,socketname)))
  ;; (multiprocessing:process-wait-until-fd-usable (socket-fd ,socketname) :input)
  )

(defmacro pro-process.wait.for.input.available (stream-or-fd whostate)

  ;;; Edited  : 24. Jul 2001 
  ;;; Authors : serge        
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+(and (not cmu) (not (and allegro-version>= (version>= 5 0))))
            `(mp:wait-for-input-available ,stream-or-fd :whostate ,whostate)
  #+(and allegro-version>= (version>= 5 0))
            `(mp:wait-for-input-available ,stream-or-fd :whostate ,whostate)
  #+LUCID   `(lcl:process-wait ,whostate #'(lambda () (peek-char nil ,stream-or-fd)))
  #+CMU     `(MULTIPROCESSING:PROCESS-WAIT ,whostate '(lambda () (peek-char nil ,stream-or-fd)))
  )


(defmacro pro-process.wait.with.timeout (whostate seconds function &rest args)

  ;;;  Input:  seconds: the number of seconds to wait
  ;;;          whostate: specifies a string that will be displayed as the state of the process
  ;;;          function: the wait function is used by the scheduler to test whether the process should run
  ;;;          args:     list of arguments to the wait function
  ;;;  Effect: Suspends the process until the function argument returns a non-nil value or the time is out.
  ;;;  Value:  The non-nil value or Nil, if timeout.

  #+ALLEGRO `(mp:process-wait-with-timeout ,whostate ,seconds ,function ,@args)
  #+LUCID   `(lcl:process-wait-with-timeout ,whostate ,seconds ,function ,@args)
  #+CMU     `(multiprocessing:process-wait-with-timeout ,whostate ,seconds ,function ,@args)
  )


(defun pro-process.activate (process)

  ;;;  Input:  A process
  ;;;  Effect: A inactive process is activated.
  ;;;  Value:  The activated process

  #+ALLEGRO (mp:process-enable process)
  #+LUCID   (lcl:activate-process process)
  #+CMU     (multiprocessing:enable-process process)
  )


(defun pro-process.deactivate (process)

  ;;;  Input:  A process
  ;;;  Effect: A active process is deactivated.
  ;;;  Value:  The deactivated process

  #+ALLEGRO  (mp:process-disable process)
  #+LUCID    (deactivate-process process)
  #+CMU      (multiprocessing:disable-process process)
  )


(defmacro pro-process.interrupt (process function &rest args)

  ;;;  Input:  process:  a process
  ;;;          function: a function
  ;;;          args:     the arguments of function
  ;;;  Effect: The execution of process is interrupted and the specified function argument is invoked.
  ;;;          The process resumes its previous computation when the interruption returns.
  ;;;  Value:  The interrupted process
  
  #+ALLEGRO  `(mp:process-interrupt ,process ,function ,@args)
  #+LUCID    `(interrupt-process ,process ,function ,@args)
  #+(and :CMU (not :cmu18))      `(multiprocessing:process-interrupt ,process ,function ,@args)
  #+CMU18       `(multiprocessing:process-interrupt ,process #'(lambda () (funcall ,function ,@args)))
  )


(defun pro-process.kill (process)

  ;;;  Input:  A process
  ;;;  Effect: The process is killed. The process cannot be restarted.
  ;;;  Value:  The killed process

  #+ALLEGRO   (mp:process-kill process)
  #+LUCID     (kill-process process)
  #+CMU       (multiprocessing:destroy-process process)
  )


(defmacro pro-with.process.lock (lock &body body)

  ;;; Edited  : 10. Mar 1999
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  #+ALLEGRO   `(mp:with-process-lock ,lock :norecursive t (progn ,@body))
  #+LUCID     `(with-process-lock ,lock (progn ,@body))
  #+CMU       `(MULTIPROCESSING:WITH-LOCK-HELD ,lock "Lock Wait" (progn ,@body))
  )


(defun pro-process.new.priority (process priority)
  (declare (edited  "26-NOV-1999")
	   (authors Sorge)
	   (input   "A process and an integer.")
	   (effect  "Sets the PROCESS priority to the PRIORITY.")
	   (value   "Undefined."))
  #+ALLEGRO   (setf (mp:process-priority process) priority)
  #-ALLEGRO   nil
  )

(defun pro-process.priority (process)
  #+ALLEGRO (mp:process-priority process)
  #-ALLEGRO   nil
  )

(defun pro-process.whostate (process)
  #+ALLEGRO (mp:process-whostate process)
  #+CMU (MULTIPROCESSING:PROCESS-WHOSTATE process)
  )

#+ALLEGRO 
(defsetf pro-process.resume.hook (process) (function)
  `(setf (mp:process-resume-hook ,process) ,function)
  )

(defun pro-process.resume.hook (process)
  #+ALLEGRO (mp:process-resume-hook process)
  #-ALLEGRO   nil
  )

(defun pro-process.name (process)
  #+ALLEGRO (mp:process-name process)
  #+CMU18   (multiprocessing:process-name process)
  )

(defun pro-process.property.list (process)
  #+ALLEGRO (mp:process-property-list process)
  #+CMU18 (multiprocessing:process-property-list process)
  )

(defsetf pro-process.property.list (process) (value)
  #+ALLEGRO `(setf (mp:process-property-list ,process) ,value)
  #+CMU18 `(setf (multiprocessing:process-property-list ,process) ,value)
  )

(defun pro-process.arrest.reasons (process)
  #+ALLEGRO (mp:process-arrest-reasons process)
  #+CMU18 (multiprocessing:process-arrest-reasons process)
  )

(defun pro-process.add.arrest.reason (process reason)
  #+ALLEGRO (mp:process-add-arrest-reason process reason)
  #+CMU18 (multiprocessing:process-add-arrest-reason process reason)
  )

(defun pro-process.revoke.arrest.reason (process reason)
  #+ALLEGRO (mp:process-revoke-arrest-reason process reason)
  #+CMU18 (multiprocessing:process-revoke-arrest-reason process reason)
  )

(defun pro-process.make.process.lock ()
  #+ALLEGRO (mp:make-process-lock)
  #-ALLEGRO   nil
  )

(defun pro-process.is (obj)
  #+ALLEGRO (mp:process-p obj)
  #+CMU18   (multiprocessing:processp obj)
  )

(defmacro pro-parallelize (&rest bodies)

  ;;; Edited  : 23. Feb 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let* ((counter 0))
    `(let (results processes)
       ,@(mapcar #'(lambda (body)
		     `(push (pro-process.create
			     :name (format nil "Parallel~D" (incf pro*counter))
			     :function #'(lambda (position)
					   (when pro*debug (format t "[~A] Starting...~%" (pro-process.name (pro-process.actual.process))))
					   (push (cons position (progn ,body)) results)
					   (when pro*debug (format t "[~A] Exiting... ~%" (pro-process.name (pro-process.actual.process))))
					   )
			     :args ,(list (incf counter)))
			    processes))
		 bodies)
       ;; Wait 'til termination of the subprocesses
       (when pro*debug (format t "[~A] Waiting for termination of subprocesses...~%" (pro-process.name (pro-process.actual.process))))
       (pro-process.wait "Waiting for termination of sub-processes"
			 #'(lambda () (every #'(lambda (process) (not (pro-process.is.active process))) processes)))
       (when pro*debug (format t "[~A] Subprocesses are all terminated...~%" (pro-process.name (pro-process.actual.process))))
       ;; Returning the results in the right order
       (mapcar #'cdr (sort results #'< :key #'car)))))


(defun pro-parallel.execution (closure args)

  ;;; Edited  : 23. Feb 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((counter 0) results processes)
    (dolist (arg args)
      (push (pro-process.create
	     :name (format nil "Parallel~D" (incf pro*counter))
	     :function #'(lambda (position arg)
			   (when pro*debug (format t "[~A] Starting...~%" (pro-process.name (pro-process.actual.process))))
			   (push (cons position (funcall closure arg)) results)
			   (when pro*debug (format t "[~A] Exiting... ~%" (pro-process.name (pro-process.actual.process))))
			   )
	     :args ((incf counter) arg))
	    processes))
    ;; Wait 'til termination of the subprocesses
    (when pro*debug (format t "[~A] Waiting for termination of subprocesses...~%" (pro-process.name (pro-process.actual.process))))
    (pro-process.wait "Waiting for termination of sub-processes"
		      #'(lambda ()
			  (every #'(lambda (process) (not (pro-process.is.active process))) processes)))
    (when pro*debug (format t "[~A] Subprocesses are all terminated...~%" (pro-process.name (pro-process.actual.process))))
    ;; Returning the results in the right order
    (mapcar #'cdr (sort results #'< :key #'car))))


(defun pro-run.foreign.process (filename &key input.type output.type error.type wait arguments)

  #+ALLEGRO (multiple-value-bind (lisp2shell-stream shell2lisp-stream shell-process-id)
		(excl:run-shell-command filename :input input.type :output output.type
					:error-output error.type :wait wait)
	      (values lisp2shell-stream shell2lisp-stream shell-process-id))
  #+CMU (let ((process (ext:run-program filename arguments 
					:input input.type 
					:output output.type 
					:error error.type
					:wait wait)))
	  (values (ext:process-input process) (ext:process-output process) (ext:process-pid process)))
  )

(defun getenv (astring)

  ;;; Edited  : 09.03.1999
  ;;; Authors : inka
  ;;; Input   : a string denoting a shell variable.
  ;;; Effect  : 
  ;;; Value   : the value of the shell variable; NIL if the shell variable does not exist.

  
  #+ALLEGRO              (sys:getenv astring)
  #+LUCID                (lcl:environment-variable astring)
  #+CMU                  (cdr (assoc (read-from-string (concatenate 'string ":" astring)) ext:*environment-list*))
  )




(defun localhost-ip-addr ()

  ;;; Edited  : 01. Mar 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((shell-stream (pro-run.foreign.process "csh" 
					       :args (list "-f")
					       :input.type :stream
					       :output.type :stream
					       :wait nil))
	tmp)
    (if (string-equal (subseq (getenv "OSTYPE") 0 5) "linux")
	(if (localhost=interface.exists? "eth0")
	    (format shell-stream "echo -n IPADDR:;ifconfig -a eth0 | grep -i \"inet addr\"  | cut -d: -f2 | cut -d\" \" -f1~%")
	  (format shell-stream "echo -n IPADDR:;ifconfig -a lo | grep -i \"inet addr\" | cut -d: -f2 | cut -d\" \" -f1~%"))
      (if (localhost=interface.exists? "le0")
	  (format shell-stream "echo -n IPADDR:;ifconfig le0 | grep -i 'inet' | cut -d\" \" -f2~%")
	(format shell-stream "echo -n IPADDR:;ifconfig lo | grep -i 'inet' | cut -d\" \" -f2~%")
	))
    (force-output shell-stream)
    (until (string= (subseq (setq tmp (read-line shell-stream)) 0 6) "IPADDR"))
    (format shell-stream "exit~%")
    (close shell-stream)
    (subseq tmp 7)))


(defun localhost=interface.exists? (interface-name)

  ;;; Edited  : 01. Mar 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (= 0 (pro-run.foreign.process "ifconfig" 
				:args (list interface-name)
				:wait t)))


(defun make-temp-file-name (&optional prefix directory)

  ;;; Edited  : 18. May 2001
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (#+(AND ALLEGRO-VERSION>= (VERSION>= 5 0))  system:make-temp-file-name
     prefix directory))


(in-package :mk)

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

(defun append-multiple-directories (dir1 dir2 &rest dirs)

  ;;; Edited  : 06. Feb 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((dir (append-directories dir1 dir2)))
    (if dirs (apply #'append-multiple-directories (cons dir dirs))
      dir)))

(defun make-package-unless-exists (packagename used-packagename)

  ;;; Edited  : 06. Feb 2003
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (unless (find-package packagename)
    (make-package packagename :use (package-use-list used-packagename))))

 
(setq *central-registry* (append-multiple-directories (mk::getenv "BIGSYSHOME") "src" "sys"))

(defparameter *dribble-file* (concatenate 'string *central-registry* "/dribble-compile"))
					  
(setq *compile-during-load* nil)

(defparameter *lisp-dependent-binary-pathname*
  (format nil "~A-~A-~A" 
	  #+(and LINUX X86)                                     'I486
	  #+(AND (OR SOLARIS SPARC) SUN)                        'SUN
	  #+(AND DARWIN PPC)                                    'PPC
	  #-(OR (AND (OR SOLARIS SPARC) SUN) 
		(AND LINUX X86)
		(AND DARWIN PPC)
		)                                               'UNKNOWN

	  #+ALLEGRO                                             'ALLEGRO
	  #+LUCID                                               'LUCID
	  #+CMU                                                 'CMU
	  #-(OR ALLEGRO LUCID CMU)                              'UNKNOWN

	  #+ALLEGRO-V5.0.1                "5-0-1"
	  #+ALLEGRO-V5.0                  "5-0"
	  #+ALLEGRO-V6.0                  "6-0"
	  #+ALLEGRO-V4.3                  "4-3"
	  #+LCL4.1                        "4-1"
	  #+LCL4.0 #-LCL4.1               "4-0"
	  #+LCL3.0 #-LCL4.0               "3-0"
	  #+CMU17                         "17"	  
          #+CMU18                         "18"
          
	  ))

(defparameter 
  #-DARWIN user::*lisp-dependent-binary-pathname* 
  #+DARWIN common-lisp-user::*lisp-dependent-binary-pathname* 
  *lisp-dependent-binary-pathname*)

(defun load-sys (system-name &optional sources-only?)
  (mk:operate-on-system system-name :load :force :all :load-source-instead-of-binary sources-only?
			))


(defun compile-sys (system-name)
  (load-sys system-name T)
  (operate-on-system system-name :compile :force :new-source-and-dependents
		     :dribble *dribble-file*))


(defparameter *lisp-source-file-type* "lisp")


(defparameter *lisp-binary-file-type*

  #+ALLEGRO                  "fasl"
  #+LUCID                    "sbin"
  #+CMU                      "fasl"
  #-(OR ALLEGRO LUCID CMU)   "bin"
  )
	  
  
(defparameter *so-compiler* 

  "ld"
;  #+LINUX86                    "ld"
;  #+(OR SOLARIS SPARC) #+SUN   "ld"
  )



(defparameter *c-compiler-shared-flags*

  "-fPIC"
;  #+LINUX86                    "-fPIC"
;  #+(OR SOLARIS SPARC) #+SUN   "-fPIC"
  )


(defparameter *c++-compiler* "c++")

(defparameter *c++-compiler-shared-flags*

  "-fPIC"
;  #+LINUX86                    "-fPIC"
;  #+(OR SOLARIS SPARC) #+SUN   "-fPIC"
  )


(defparameter *so-compiler-shared-flags* 

  "-G"
;  #+LINUX86                    "-G"
;  #+(OR SOLARIS SPARC) #+SUN   "-G"
  )

(defun c-so-compile-file (filename &rest args &key output-file)
  (declare (ignore args))
  ;; deleting the intermediate file if it exists.
  (when (probe-file (format nil "~A.o" filename))
    (delete-file (format nil "~A.o" filename)))
  ;; compiling the c-source file
  ;; gcc -c foo.c -o foo.o $PICFLAG
  (run-unix-program *c-compiler* 
		    #+CMU (list "-c" filename "-o" (format nil "~A.o" filename) *c-compiler-shared-flags*)
		    #-CMU (format nil "-c ~A -o ~A.o ~A" filename filename *c-compiler-shared-flags*)
		    )
  ;; ld -o foo.so foo.o $SHAREFLAG
  (run-unix-program *so-compiler*
		    #+CMU (list "-o" output-file (format nil "~A.o" filename) *so-compiler-shared-flags*)
		    #-CMU (format nil "-o ~A ~A.o ~A" output-file filename *so-compiler-shared-flags*)
		    )
  ;; deleting the intermediate file
  (when (probe-file (format nil "~A.o" filename))
    (delete-file (format nil "~A.o" filename)))
  )

(define-language :c-so
  :compiler #'c-so-compile-file
  :loader #+lucid #'load-foreign-files 
          #+allegro #'load
	  #+CMU #'alien:load-foreign
          #-(or lucid allegro cmu) #'load
  :source-extension "c"
  :binary-extension "so"
  :binary-load-flag t)


(defun c++-so-compile-file (filename &rest args &key output-file)
  (declare (ignore args))
  ;; deleting the intermediate file if it exists.
  (when (probe-file (format nil "~A.o" filename))
    (delete-file (format nil "~A.o" filename)))
  ;; compiling the c-source file
  ;; gcc -c foo.c -o foo.o $PICFLAG
  (run-unix-program *c++-compiler*
		    #+CMU (list "-c" "-shared" filename "-o" (format nil "~A.o" filename) *c++-compiler-shared-flags*)
		    #-CMU (format nil "-c -shared ~A -o ~A.o ~A" filename filename *c++-compiler-shared-flags*)
		    )
  ;; ld -o foo.so foo.o $SHAREFLAG
  (run-unix-program *so-compiler*
		    #+CMU (list "-o" output-file (format nil "~A.o" filename) *so-compiler-shared-flags*)
		    #-CMU (format nil "-o ~A ~A.o ~A" output-file filename *so-compiler-shared-flags*)
		    )
  ;; deleting the intermediate file
  (when (probe-file (format nil "~A.o" filename))
    (delete-file (format nil "~A.o" filename)))
  )


(define-language :c++-so
  :compiler #'c++-so-compile-file
  :loader #+lucid #'load-foreign-files 
          #+allegro #'load
	  #+CMU #'alien:load-foreign
          #-(or lucid allegro cmu) #'load
  :source-extension "cc"
  :binary-extension "so"
  :binary-load-flag t)

(define-language :lisp
  :compiler #'compile-file
  :loader #'load
  :source-extension *lisp-source-file-type*
  :binary-extension *lisp-binary-file-type*)

(defun bongrammar-compile-file (filename &rest args &key output-file)
  (declare (ignore args))
  (compile-sys 'bonasus-compiler)
  (load-sys 'bonasus-compiler)
  (let* ((*g-spec* (funcall (intern (symbol-name :read-spec-from-file) :bon-comp) filename))
	 (*pt* (funcall (intern (symbol-name :make-parsetable) :bon-comp) 
			(funcall (intern (symbol-name :compile-grammar) :bon-comp) *g-spec* :quiet t))))
    (funcall (intern (symbol-name :save-parsetable) :bon-comp) *pt* output-file)))


(define-language :bongrammar
  :compiler #'bongrammar-compile-file
  :loader #'(lambda (filename) (declare (ignore filename)) t)
  :source-extension "bg"
  :binary-extension "parsetable"
  :binary-load-flag t
  )


(defun bonlexer-compile-file (filename &rest args &key output-file)
  (declare (ignore args))
  (compile-sys 'bonasus-compiler)
  (load-sys 'bonasus-compiler)
  (let* ((*l-spec* (funcall (intern (symbol-name :read-spec-from-file) :blex-comp) filename))
	 (*lt* (funcall (intern (symbol-name :make-lextable) :blex-comp) 
			(funcall (intern (symbol-name :compile-lexer) :blex-comp) *l-spec* :quiet t))))
    (funcall (intern (symbol-name :save-lextable) :blex-comp) *lt* output-file)))


(define-language :bonlexer
  :compiler #'bonlexer-compile-file
  :loader #'(lambda (filename) (declare (ignore filename)) t)
  :source-extension "bl"
  :binary-extension "lextable"
  :binary-load-flag t)


(defun dump-system (system-name)

  ;;; Edited  : 07. Feb 2003
  ;;; Authors : serge       
  ;;; Input   : a system-name
  ;;; Effect  : writes a dump for that system
  ;;; Value   : nil
  
  (let ((system (find-system system-name :load)))
    (if system
	(let* ((dumppath (append-multiple-directories
			  (getenv "BIGSYSHOME")
			  "binaries"
			  *lisp-dependent-binary-pathname*))
	       (filename (append-multiple-directories dumppath
						      (string-downcase (format nil "~A.dxl" system-name))))
	      (init-function (eval (component-dump-init-function system))))
	  (unless (probe-file dumppath)
	    (warn "Creating full binary pathname ~A" dumppath)
	    (run-unix-program "mkdir" 
			      #+CMU (list "-p" dumppath)
			      #-CMU (concatenate 'string "-p " dumppath)
			      ))
	  (if init-function
	      (dump=system system-name filename init-function)
	    (error "Cannot dump systems without dump-init-function!")))
      (error "Can't find system named ~s." system-name))))


      
(defun dump=system (system filename init-function &optional startup-function)
;;; The function DUMP-SYSTEM should be used to save a dump on disk.
;  (declare (edited  "18-SEP-1996" "20-JUL-1993 13:53" )
;        (authors Fehrer NESMITH )
;        (input   "A system name, a filename for the dump and a 
;funcallable startup-function")
;        (effect  "The system is loaded, and a LISP image for KEIM is dumped.")
;        (value   "Undefined."))
  #+cmu (declare (ignore startup-function))
  #+symbolics
  (error "Don't know how to dump Symbolics.")
  #-symbolics
  (progn
    (mk:operate-on-system system :load :verbose t)
    #+(or lcl4.0 :lcl4.1)
    (lcl:disksave filename
		  :restart-function startup-function
                  :full-gc t)
    #+(or allegro-v4.1 allegro-v4.2)
    (progn
      (setq excl:*cl-default-special-bindings*
	    (remove '*package*
		    (remove '*readtable* excl:*cl-default-special-bindings*
			    :key #'car)
		    :key #'car))
      (excl:gc t)
      (let ((excl:*libfasl* nil))
	(sys:presto-build-lib (namestring (make-pathname :name filename :type "additives")))
	(excl:dumplisp :name filename :checkpoint nil
		       :read-init-file nil
		       :restart-function startup-function)))
    #+(or allegro-v4.3 allegro-v4.3.1 allegro-v5.0 allegro-v5.0.1 allegro-v6.0)
    (progn
      (setq excl:*cl-default-special-bindings*
	    (remove '*package*
		    (remove '*readtable* excl:*cl-default-special-bindings*
			    :key #'car)
		    :key #'car))
      (excl:gc t)
      (setq excl:*restart-init-function* init-function)
      #-(or allegro-v5.0.1 allegro-v6.0) (setq excl:*restart-actions* (list startup-function))
      (let ((excl:*libfasl* nil)
	    (excl:*restart-init-function* init-function)
	    (excl:*restart-actions* (list startup-function)))
	(sys:presto-build-lib (namestring (make-pathname :name filename :type "additives")))
	(excl:dumplisp :name filename :checkpoint nil)))
    #+gcl
    (si::save-system filename)
    #+:cmu
    (progn 
      #-darwin (ext:gc :full t)
      #+darwin (ext:gc :full)
      (ext:save-lisp filename
		     :init-function init-function :site-init nil :print-herald nil)
      )
    #+ccl-2
    (ccl:save-application filename :toplevel-function startup-function)
    #+clisp3
    (progn
      (lisp:gc) (lisp:saveinitmem filename :init-function startup-function))
    ))

(defun systems-in-system-and-dependents (name &optional (force :all)
					      (type :source) version)
  ;; Returns a list of the symbolic names of the system and dependents in load order.
  (let ((system (find-system name :load)))
    (multiple-value-bind (*version-dir* *version-replace*) 
	(translate-version version)
      (let ((*version* version))
	(let ((result (list (canonicalize-system-name name))))
	  (dolist (dependent (reverse (component-depends-on system)))
	    (setq result 
		  (append (systems-in-system-and-dependents dependent
							    force type version)
			  result)))
	  result)))))

(proclaim '(optimize (speed 3) (safety 1) (compilation-speed 0)))


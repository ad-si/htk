(defun getenv (astring)

  ;;; Edited  : 09.03.1999
  ;;; Authors : inka
  ;;; Input   : a string denoting a shell variable.
  ;;; Effect  : 
  ;;; Value   : the value of the shell variable; NIL if the shell variable does not exist.
  
  #+ALLEGRO              (sys:getenv astring)
  #+LUCID                (lcl:environment-variable astring)
  #+CMU                  (cdr (assoc (read-from-string (concatenate 'string ":" astring)) 
				     ext:*environment-list*))
  )

(load (format nil "~A/src/sys/defsystem.lisp" (getenv "BIGSYSHOME")))
(load (format nil "~A/src/sys/config-defsystem.lisp" (getenv "BIGSYSHOME")))

(let ((script-filename (format nil "~A/bin/set-lisp-switches" (getenv "BIGSYSHOME"))))
  (with-open-file (script-file script-filename
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create)
		  (format script-file "setenv LISPAPPARGSSWITCH ~S~%" 
			  #+ALLEGRO     " -- " 
			  #+CMU       "" 
			  )
		  (format script-file "setenv LISPCORESWITCH ~S~%" 
			  #+ALLEGRO     " -I " 
			  #+CMU       " -core " 
			  )
		  (format script-file "setenv FORMEVALSWITCH ~S~%"  
			  #+ALLEGRO     " -e " 
			  #+CMU       " -eval " 
			  )
		  (format script-file "setenv BIGSYSBINARYPATHNAME ~S~%"
			  make::*lisp-dependent-binary-pathname*)
		  (format script-file "setenv LISPIMPLEMENTATION ~S~%" 
			  #+ALLEGRO  "ALLEGRO"
			  #+CMU      "CMU"
			  )
		  (force-output script-file)
		  ))

		  


(in-package :inka)


;; ---------------------------------------------------------------------------
;; Section 0 : foreign function definitions for base64 encoding
;; ---------------------------------------------------------------------------

#+ALLEGRO
(progn
  (ff:defforeign 'b64decode :arguments '(string integer string integer)
		 :pass-types '(:by-value :by-value :by-value :by-value))
  (ff:defforeign 'b64encode :arguments '(string integer string integer integer)
		 :pass-types '(:by-value :by-value :by-value :by-value :by-value))
  )
	       
;(progn
;  (ff:def-foreign-call (b64decode "b64decode")
;		      ((simple-array simple-string (*))
;		       integer
;		       (simple-array simple-string (*))
;		       integer
;		      ))
;  (ff:def-foreign-call (b64encode "b64encode")
;		       ((x (* :char) string) (y :fixnum) (z (* :char) string) (u :fixnum) (v :fixnum)))
;  )
	       
#+CMU 
(progn 
  (alien:def-alien-routine 'b64decode c-call:void
			   (input c-call:c-string)
			   (i-length alien:unsigned :in)
			   (output c-call:c-string)
			   (o-length alien:unsigned :in))
  (alien:def-alien-routine 'b64encode c-call:void
			   (input c-call:c-string)
			   (i-length alien:unsigned :in)
			   (output c-call:c-string)
			   (o-length alien:unsigned :in)
			   (linelength integer :in)
			   )
  )
  
			   
;; ---------------------------------------------------------------------------
;; Section 1 : The interface functions
;; ---------------------------------------------------------------------------

(defun b64-decode.string (input)

  ;;; Edited  : 01. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (assert (stringp input))
  (let ((output (make-string (b64=string.decoded.length input) :initial-element #\#)))
    (b64decode input (length input) output (length output))
    ;; (base64:base64-decode input)
    output
    ))


(defun b64-encode.string (input &optional (linelength 60))

  ;;; Edited  : 01. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (assert (stringp input))
  (let* ((encodedsize (b64=string.encoded.length input linelength))
	 (output (make-string encodedsize :initial-element #\#)))
    (b64encode input (length input) output encodedsize linelength)
    output
    ;; (base64:base64-encode input)
    ))


(defparameter b64*datachars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")


(defun b64=string.decoded.length (input)

  ;;; Edited  : 01. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 

  (let ((datachars 0) (fillchars 0))
    (do ((i 0))
	((> i (length input)))
      (if (eq (elt input i) #\=)
	  (incf fillchars)
	(when (find (elt input i) b64*datachars)
	  (incf datachars)))
      (incf i)
      )
    (when (< 0 (mod (+ datachars fillchars) 4)) (error "BASE64-encoding has ~D relevant characters, which is not divisible by 4.~%"
						       (+ datachars fillchars)))
    (setq fillchars (mod fillchars 4))
    (- (* 3 (truncate (/ (+ datachars fillchars) 4))) fillchars)))


(defun b64=string.encoded.length (input &optional (linelength 60))

  ;;; Edited  : 01. Sep 2000
  ;;; Authors : serge       
  ;;; Input   : 
  ;;; Effect  : 
  ;;; Value   : 
  
  (let ((targetsize (* (truncate (/ (length input) 3)) 4)))
    (when (> (mod (length input) 3) 0) (incf targetsize 4))
    (when (> linelength 0) (setq targetsize (+ targetsize (truncate (/ targetsize linelength)))))
    targetsize))

;;;;
;;;; This is an XEmacs lisp preamble to be executed at the start of
;;;; execution of XEmacs when editing uni code 
;;;;
;;;; All top-level identifiers begin with "uni-".
;;;; 

;;;;
;;;; Note on scoping.  XEmacs has no notion of closure and uses what it
;;;; calls dynamic scoping; this means that variable bindings come in and out 
;;;; of scope based purely on when bindings are executed.  This has many
;;;; surprising consequences; for example if a function argument shares
;;;; a name with some variable accessed by some function called from within
;;;; that function, the inner function will actually read the function 
;;;; argument (rather than a global variable for example).
;;;;
;;;; To deal with these tiresome rules we adopt the following conventions.
;;;; (1) Functions in the (autoloaded) package cl-macs are used to provide
;;;;     lexical scoping; in particular lexical-let and lexical-let*.
;;;; (2) Many variables are declared local to the buffer of the process
;;;;     attached to the network "process" which communicates with Haskell.
;;;;     This process is also the one the user is actually editing.
;;;;     (Morally we should be using it for storing Haskell's output, but
;;;;     XEmacs doesn't seem to mind.)
;;;; (3) Names of the form uni-haskell-* are available for global variables
;;;;     created by Haskell.  No function argument should have a name of
;;;;     this form.  
;;;; (4) All symbols defined at top-level in this module have names
;;;;     beginning "uni-".

;;;
;;;  Initialisation
;;;

; (uni-initialise PORT KEY NAME) is the function called (via gnuclient)
; from Haskell to start the thing off.  So it has the following to do.
; (1) create a new buffer with name based on NAME.
; (2) open a connection to Haskell associated with this buffer on the given
;     network PORT.
; (3) set up any other variables we need.
; (4) define a filter function taking output from the process and executing
;     it as commands.  (The commands will also be able to write back along
;     the network connection.)  All commands are executed in the 
;     uni-buffer.
; (5) send KEY over the connection; this simultaneously (a) identifies us
;     to Haskell; (b) tells Haskell we are open and ready for business.
;
; We don't open a new frame or load a file into the buffer; that we leave to
; Haskell.

(defun uni-initialise (port key name)
   (let* ((uni-buffer (generate-new-buffer name)))
      (uni-set-buffer uni-buffer
         (lambda () 
            (progn
               ;; (2)
               (make-local-variable 'uni-process)
               (setq uni-process (open-network-stream "uni-server" uni-buffer 
                  "localhost" port))
               ;; (3)
               ; The line as it has so far been processed
               (make-local-variable 'uni-half-line)
               (setq uni-half-line "")
               ;; (4)
               (set-process-filter uni-process 'uni-exec-filter)
               ;; (5)
               (uni-ps key)
               )
            )
         )
      )
   )


; The filter program.  
(defun uni-exec-filter (process command) 
   (uni-set-buffer 
      (process-buffer process)
      (lambda () (uni-passon-command command))
      )
   )

;;;
;;; The current buffer.
;;;

; Change to the given buffer, and perform the action, then change back.
; This also disables uni-check-change
(defun uni-set-buffer (buffer action)
   (save-excursion
      (set-buffer buffer)
      (let ((uni-allow-changes t)) (funcall action))
      )
   )

;;;
;;; Arrange the Haskell process to send Emacs lisp code which will
;;; be executed.  Each line should contain one or more complete
;;; forms to be executed. 
;;;

; Wrapped version of uni-exec-string which reports errors
(defun uni-exec-string-safe (string)
   (condition-case err
      (uni-exec-string string)
      (error (uni-er (format "%s in command %s" err string)))
      )
   )


; Execute a string containing a sequence of Emacs lisp commands.
(defun uni-exec-string (string) (uni-exec-string-general string 0 (length string)))

(defun uni-exec-string-general (string start end)
   (if (< start end)
      (let* (
            (parsed (read-from-string string start end))
            (form (car parsed))
            (next (cdr parsed))
            )
         (progn 
            (eval form)
            (uni-exec-string-general string next end)
            )
         )
      )
   )

;
; Code which builds up the commands from the pieces as they arrive
; to the filter command.
; 

; process a new bit of line
(defun uni-passon-command (command) 
   (let* (
         (command-parts0 (uni-split command ?\n))
         (command-parts1 (cons 
            (concat uni-half-line (car command-parts0))
            (cdr command-parts0)
            ))
         (last-index (- (length command-parts1) 1))
         (new-half-line (nth last-index command-parts1))
         )
      (progn
         (setq uni-half-line new-half-line)
         (uni-appn 'uni-exec-string-safe command-parts1 last-index)
         )
      )
   )

;;;
;;; Utility functions
;;;


; Send a normal response to a command
(defun uni-ok (string) (uni-ps (concat "OK " (uni-escape string))))

; Send a response to a command where we are sure that the String doesn't
; need escaping (has no newlines) and don't escape it.  We also
; avoid doing any concat operations, to save time.
(defun uni-ok-quick (string) 
   (progn
      (process-send-string uni-process "OK ")
      (process-send-string uni-process string)
      (process-send-string uni-process "\n")
      )
   )

; Send an event
; An event has a key and a value string. 
; The key is assumed to contain no space characters (newlines or blanks).
(defun uni-ev (key string) 
   (progn
      (process-send-string uni-process "EV ")
      (process-send-string uni-process key)
      (process-send-string uni-process " ")
      (process-send-string uni-process (uni-escape string))
      (process-send-string uni-process "\n")
      )
   )

; Send an error message
(defun uni-er (string) (uni-ps (concat "ER " (uni-escape string))))

; General functions for sending a message to Haskell
(defun uni-ps (string) (process-send-string uni-process (concat string "\n")))

; Escape a string.  For fear that Haskell won't understand some XEmacs 
; escapes, we only do the escapes that are absolutely necessary, namely
; newline -> \n and \ -> \\
(defun uni-escape (string)
   (mapconcat
      (lambda (ch)
         (cond 
            ((eq ch ?\n) "\\n")
            ((eq ch ?\\) "\\\\")
            (t (char-to-string ch))
            )
         )
      string
      ""
      )
   )

; Describe an object for Haskell
(defun uni-prin (s) (let ((print-escape-newlines t)) (prin1-to-string s)))

; Apply f to the first n elements of l
(defun uni-appn (f l n)
   (if (> n 0) 
      (progn
         (funcall f (car l))
         (uni-appn f (cdr l) (- n 1))
         )
      )
   )

; Search the sequence for the first element equal to ch
(defun uni-scan (s start end ch)
   (if (< start end)
      (if (eq (elt s start) ch)
         start
         (uni-scan s (+ start 1) end ch)
         )
      nil
      )
   )

; Split the given string at points indicated by the given character.
(defun uni-split (s ch)
   (uni-split-general s 0 (length s) ch)
   )

(defun uni-split-general (s start end ch)
   (let ((next (uni-scan s start end ch)))
      (if next
          (cons
             (substring s start next)
             (uni-split-general s (+ next 1) end ch)
             )
          (list (substring s start end))
          )
      )
   )

; Explode the given string
(defun uni-explode (l) (mapcar 'char-to-string l))

; Produce an ASCII representation of the given string
(defun uni-ascii (s) (mapcar 'char-int (uni-explode s)))


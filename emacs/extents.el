;;;;
;;;;
;;;; This file contains code for manipulating extents and containers
;;;; in MMiSS buffers.
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
;;; Libraries
;;;
(load-library "atomic-extents")


;;;
;;;  Initialisation
;;;
;;; uni-initialise-extents should be done immediately after the mode
;;; is set up (not before as it relies on buffer local variables).

(defun uni-initialise-extents ()
   ; The hash table from extent-ids to extents. 
   ; Keys are strings, values are extents.
   (make-local-variable 'uni-extent-hash-table)
   (setq uni-extent-hash-table 
      (make-hash-table :size 200 :test 'equal
         :weakness 'value))

   ; Set the uni-check-change function to prevent changes in
   ; illicit parts of the buffer.
   (make-local-variable 'before-change-functions)
   (setq before-change-functions
      (cons 'uni-check-change before-change-functions))

   ; Set variable indicating if we are to have coloured magic buttons
   ; (No until uni-set-colour-hack is called)
   (make-local-variable 'uni-do-colour-hack)
   )


;;;
;;; Extents
;;; 

; We define two sorts of extents:
; 
; button extents
; container extents
; 
; A button extent is an extent containing an uneditable text, which when 
; button1 is pressed generates Haskell events (keyed by the identifier).  
;    Parameters are the text of the label and a unique identifier.
;
;    A button extent is open at both ends (so inserting a character at either
;    end will cause the character to appear in the neighbouring text, not
;    in the button).
;
; A container extent is an extent containing buttons and text.
;    Contents are the sequence of extents, with text extents identified by
;    their current text, and button and container extents by their identifier.
;
;    A container extent is  open at the left end and closed at the right
;    until uni-bound-container is called, which opens it also at the right.
;    This is necessary if further containers are to be added at the right.
;   
;    We also add an extent containing a single invisible "#" to the start of
;    each container.
;
; The buffer is a contiguous sequence of container extents.  It starts out
; empty.
;
; If the variable uni-do-colour-hack is set, we assume the
;   last letter of the extent id of a button is one recognised by the 
;   function MMiSS-retrieve-face, and use that function to obtain the face 
;   for the button extent.  This variable is set by the following function:
(defun uni-set-colour-hack () 
   (setq uni-do-colour-hack t)
   )

;
; High-level extent functions (to be used from Haskell)
;

; Append a container to the buffer
(defun uni-add-container-buffer (extent-id)
   (uni-create-container-extent extent-id (+ (buffer-size) 1))
   )

; Ditto only prepend
(defun uni-prepend-container-buffer (extent-id)
   (uni-create-container-extent extent-id 1)
   )

; Append a container to a container
(defun uni-add-container (parent-extent-id extent-id)
   (let* (
         (parent (gethash parent-extent-id uni-extent-hash-table))
         (parent-end (extent-end-position parent))
         )
      (uni-create-container-extent extent-id parent-end)
      )
   )

; Append a button to a container
(defun uni-add-button (parent-extent-id extent-id text)
   (let* (
         (parent (gethash parent-extent-id uni-extent-hash-table))
         (parent-end (extent-end-position parent))
         )
      (uni-create-button-extent extent-id parent-end text)
      )
   )

; Add editable text to a container
(defun uni-add-text (parent-extent-id text)
   (let* (
         (parent (gethash parent-extent-id uni-extent-hash-table))
         (parent-end (extent-end-position parent))
         )
      (goto-char parent-end)
      (insert text)
      )
   )

; Add a boundary to a container.  This closes the 
; container to further additions.
(defun uni-bound-container (parent-extent-id)
   (let* (
         (parent (gethash parent-extent-id uni-extent-hash-table))
         )
      (set-extent-property parent 'end-open t)
      )
   )

; Delete a button or container extent
(defun uni-delete-extent (extent-id)
   (let* (
         (extent (gethash extent-id uni-extent-hash-table))
         (extent-start (extent-start-position extent))
         (extent-end (extent-end-position extent))
         )
      (delete-region extent-start extent-end)
      ;  Because this and all contained extents are detachable this removes 
      ;  them from the buffer.
      ;  
      ;  Because the hash table is value-weak, this will mean the entries
      ;  can also disappear, when the extents are gc'd.
      )
   )

; Expand a button extent to a container (with the same id)
(defun uni-expand (extent-id) 
   (let* (
         (extent (gethash extent-id uni-extent-hash-table))
         (extent-start (extent-start-position extent))
         (extent-end (extent-end-position extent))
         )
      (delete-region extent-start extent-end)
      (let
         ((new-extent (uni-create-container-extent extent-id extent-start)))
;         (puthash extent-id new-extent uni-extent-hash-table)
         )
      )
   )

; Collapse a container extent to a button (with the same id)
(defun uni-collapse (extent-id button-text)
   (let* (
         (extent (gethash extent-id uni-extent-hash-table))
         (extent-start (extent-start-position extent))
         (extent-end (extent-end-position extent))
         )
      (delete-region extent-start extent-end)
      (let
         ((new-extent 
            (uni-create-button-extent extent-id extent-start button-text)))
;         (puthash extent-id new-extent uni-extent-hash-table)
         )
      )
   )

; Extract the contents of a container extent.
; This is a list containing Strings and dotted pairs (symbol . String or ()).
; The Strings represent text in the container
; The pairs (symbol . String or ()) represent included extents.  The symbol
; indicates the type ('button, 'container or 'boundary).  For button and
; container extents the String gives the identifier; for boundary extents
; null is provided.
(defun uni-container-contents (extent-id)
   (let* (
         (container (gethash extent-id uni-extent-hash-table))
         (list-so-far nil)
         (last-text (+ (extent-start-position container) 1))
         (container-end (extent-end-position container))
         )
      ; We build up the list in list-so-far using map-extent-children
      ; It is built up in reverse order
      (map-extent-children
         (lambda (extent maparg)
            (let  (
                  (extent-start (extent-start-position extent)) 
                  (extent-end (extent-end-position extent))
                  )
               (cond 
                  ((< last-text extent-start)
                     (setq list-so-far (cons (buffer-string last-text 
                        extent-start) list-so-far)))
                  )
               (setq last-text extent-end)
               (setq list-so-far 
                  (cons (uni-get-extent-descriptor extent) list-so-far))
               nil
               )
            )
         container nil nil nil nil 'uni-extent-type
         )
      (cond 
         ((< last-text container-end) 
            (setq list-so-far (cons (buffer-string last-text container-end)
               list-so-far)))
         )
      (nreverse list-so-far)
      )
   )

; Used to construct dotted pair for uni-container-contents
(defun uni-get-extent-descriptor (extent) 
   (cons (uni-get-extent-type extent) (uni-get-extent-id extent))
   )

;
; Function that lists all container extents in the buffer
;
(defun uni-list-containers () 
   (mapcar-extents 'uni-get-extent-id nil nil nil nil nil 
      'uni-extent-type 'container)
   )
;
; Basic extent functions
;

; Create a new container extent.  We put an invisible # character
; in at the start. 
(defun uni-create-container-extent (extent-id pos)
   (let 
      ((uni-new-extent (uni-create-extent extent-id pos 'container)))
      (set-extent-property uni-new-extent 'start-closed t)
      (set-extent-property uni-new-extent 'end-closed t)
      (uni-create-marker-extent pos)
      (set-extent-property uni-new-extent 'start-open t)
      (set-extent-property uni-new-extent 'end-closed t)
      )
   )

(defun uni-create-marker-extent (pos)
   (let ((new-extent (make-extent pos pos)))
      (set-extent-property new-extent 'start-closed t)
      (set-extent-property new-extent 'end-closed t)
      (set-extent-property new-extent 'invisible t)
      (goto-char pos)
      (insert "#")
      (set-extent-property new-extent 'start-open t)
      (set-extent-property new-extent 'end-open t)
      )
   )

(defun uni-create-boundary-extent (pos boundary-text)
   (let 
      ((uni-new-extent (uni-create-extent nil pos 'boundary)))
      (set-extent-property uni-new-extent 'start-closed t)
      (set-extent-property uni-new-extent 'end-closed t)
      (goto-char pos)
      (insert boundary-text)
      (set-extent-property uni-new-extent 'start-open t)
      (set-extent-property uni-new-extent 'end-closed t)
      (set-extent-property uni-new-extent 'atomic t)
      )
   )

; Create a new button extent with a given text
(defun uni-create-button-extent (extent-id pos text)
   (let 
      ((uni-new-extent (uni-create-extent extent-id pos 'button)))
      (set-extent-property uni-new-extent 'start-closed t)
      (set-extent-property uni-new-extent 'end-closed t)
      (goto-char pos)
      (insert text)
      (set-extent-property uni-new-extent 'start-open t)
      (set-extent-property uni-new-extent 'end-open t)
      (set-extent-property uni-new-extent 'face 
         (if uni-do-colour-hack
            (MMiSS-retrieve-face (elt extent-id (- (length extent-id) 1)))
            'highlight
            )
         )
      (set-extent-property uni-new-extent 'atomic t)
      (set-extent-property uni-new-extent 'keymap uni-extent-keymap)
      )
   )

; Create an extent with the given extent-id, position and type
(defun uni-create-extent (extent-id pos type)
   (let
      ((uni-new-extent (make-extent pos pos)))
      (set-extent-property uni-new-extent 'uni-extent-type type) 
      (set-extent-property uni-new-extent 'uni-extent-id extent-id)
      (puthash extent-id uni-new-extent uni-extent-hash-table)
      )
   )
;
;
; uni-check-change is a change hook added to the current buffer to determine
; whether to permit the change or not.
;
; Setting uni-allow-changes disables the check
(setq uni-allow-changes nil)

(defun uni-check-change (from to)
   (cond 
      ((null uni-allow-changes)
         (cond
            ((eq from 1) (error "Cannot modify start of a buffer"))
            ((eq from to) ;; this is an insertion
               (let* (
                     (closest-extent (extent-at from nil 'uni-extent-type))
                     (extent-type (uni-get-extent-type closest-extent))
                     )
                  (cond
                     ((null closest-extent) 
                        (error "Can't modify end of buffer"))
                     ((eq extent-type 'container) 
                        (cond 
                           ((eq from (extent-start-position closest-extent))
                              (error "Can't add between containers"))
                           (t ())
                           )
                        )
                     ((eq (extent-start-position closest-extent) from) t)
                     ;; OK because inserting at the start of a button
                     ;; boundary extent 
                     ;; simply inserts the text before the extent
                     (t (error "Cannot insert into a button"))
                     )
                  )
               )
            (t ;; this is a query-replace or deletion.
               (let* (
                     (extent-from (extent-at from nil 'uni-extent-type))
                     (extent-to 
                        (extent-at to nil 'uni-extent-type nil 'before))
                     )
                  (cond 
                     (  (and (eq extent-from extent-to) 
                        (eq (uni-get-extent-type extent-from) 'container)
                        ) 
                        (map-extents 
                           (lambda (extent arg) 
                              (error "Change includes extents"))
                           nil from to nil 'start-and-end-in-region 
                           'uni-extent-type
                           )
                        )
                     (t (error "Change crosses extent boundary"))
                     )
                  )
               )
            )
         )
      (t ())
      )
   )
             
       

; Get an extent's id
(defun uni-get-extent-id (extent)
   (plist-get (object-plist extent) 'uni-extent-id)
   )

; Get an extent's type
(defun uni-get-extent-type (extent)
   (plist-get (object-plist extent) 'uni-extent-type)
   ) 

;;;
;;; The special keymap that means button clicks on button extents 
;;; get turned into Haskell events. 
;;;

(setq uni-extent-keymap (make-sparse-keymap))

; Function to be attached to keymap which finds out what extent to
; use, extracts the "uni-extent-id" property, and sends it to Haskell.
; The uni-last-extent variable is set to the extent, so if Haskell sends
; further Lisp code it can refer to it.
(defun uni-button-action ()
   (interactive)
   (let* ( 
      (event (next-command-event))
      (pos (event-point event))
      (extent (extent-at pos))
      )
      (if extent
         (if (eq (event-button event) 1)
            (progn
               (setq uni-last-extent extent)
               (uni-ev "BUTTON" (uni-get-extent-id extent))
               )
            )
         (dispatch-event event)
         )
      )
   )

(define-key uni-extent-keymap [button1] 'uni-button-action)

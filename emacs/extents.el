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

   ; Set counter indicating how many times Haskell wants to lock this
   ; buffer, to prevent the user from making changes to it.  (Normally I
   ; suppose this variable will be 0 or 1.)
   (make-local-variable 'uni-buffer-lock-counter)
   (setq uni-buffer-lock-counter 0)
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

; Append a head button to a container
(defun uni-add-head-button (parent-extent-id extent-id text)
   (uni-add-button parent-extent-id extent-id text)
   (uni-mk-head-button extent-id)
   )

; Add uneditable text to the container.  The face of the new text is
; the same as that a button with the container's name would have.
(defun uni-add-uneditable (parent-extent-id text)
   (let* (
         (parent (gethash parent-extent-id uni-extent-hash-table))
         (parent-end (extent-end-position parent))
         )
      (uni-create-uneditable-extent parent-extent-id parent-end text)
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
;
; duplicates code with uni-container-children.
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

; uni-container-children is like uni-container-contents, except that
; it doesn't return the text, just the included extents.
;
; duplicates code with uni-container-contents.
(defun uni-container-children (extent-id)
   (let* (
         (container (gethash extent-id uni-extent-hash-table))
         (list-so-far nil)
         )
      ; We build up the list in list-so-far using map-extent-children
      ; It is built up in reverse order
      (map-extent-children
         (lambda (extent maparg)
            (setq list-so-far 
               (cons (uni-get-extent-descriptor extent) list-so-far))
            nil
            )
         container nil nil nil nil 'uni-extent-type
         )
      (nreverse list-so-far)
      )
   )


; Used to construct dotted pair for uni-container-contents
(defun uni-get-extent-descriptor (extent) 
   (cons (uni-get-extent-type extent) (uni-get-extent-id extent))
   )

; Retrieves the type of an extent
(defun uni-get-extent-id-type (extent-id)
   (let* (
         (extent (gethash extent-id uni-extent-hash-table))
         )
      (uni-get-extent-type extent)
      )
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

; Create an uneditable extent with the given text.  This is very similar
; to uni-create-button-extent except (1) we do not put the extent in the
; hash table; (2) there is no special action when the text is clicked.
(defun uni-create-uneditable-extent (extent-id pos text)
   (let 
      ((uni-new-extent (uni-create-extent nil pos 'uneditable-extent)))
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
   (if (null uni-allow-changes)
      (cond
         ((> uni-buffer-lock-counter 0) 
            (error "Buffer is locked by Haskell"))
         ((eq from 1) (error "Cannot modify start of a buffer"))
         ((eq from to) ;; this is an insertion
            (let ((closest-extent (extent-at from nil 'uni-extent-type)))
               (cond
                  ((null closest-extent) 
                     (error "Can't modify end of buffer"))
                  ((eq (uni-get-extent-type closest-extent) 'container) 
                     (cond 
                        ((eq from (extent-start-position closest-extent))
                           (error "Can't add between containers"))
                        (t (uni-modify-extent closest-extent))
                        )
                     )
                  ((eq (extent-start-position closest-extent) from) 
                      (if (uni-is-head-button closest-extent) 
                        (error "Can't insert before a head button")

                        ; The following extent-at incantation finds the
                        ; container extent containing this button.  We need
                        ; to avoid (a) picking a button extent immediately
                        ; before closest-extent; that is why we specify 'after;
                        ; (b) picking a container extent which includes the
                        ; one we actually want; that is why we use extent-at,
                        ; which returns the smallest matching extent; (c)
                        ; picking closest-extent itself; that's why we specify
                        ; it as the 4th argument.
                        (let ((container-extent
                                (extent-at from (current-buffer) 
                                'uni-extent-type closest-extent `after)))
                           (uni-modify-extent container-extent)
  
                           ;; we are at the start of a button extent which
                           ;; does not have the uni-head-button property
                           ;; set.  Text inserted here will go before the
                           ;; start of the button.
                           )
                        )
                     )
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
                     (progn 
                        (map-extents 
                           (lambda (extent arg) 
                              (error "Change includes extents"))
                           nil from to nil 'start-and-end-in-region 
                           'uni-extent-type
                           )
                        (uni-modify-extent extent-from)
                        )
                     )
                  (t (error "Change crosses extent boundary"))
                  )
               )
            )
         )
      ()
      )
   )

; Give an extent the uni-head-button property.  This means
; uni-check-changes stops insertions just before the extent.
(defun uni-mk-head-button (extent-id)
   (let ((extent (gethash extent-id uni-extent-hash-table)))
      (set-extent-property extent 'uni-head-button t)
      )
   )

(defun uni-is-head-button (extent)
   (extent-property extent 'uni-head-button)
   )


; Mark an extent as modified.
(defun uni-modify-extent (extent)
   (set-extent-property extent 'uni-edited t)
   )

; Determine if the container extent is modified (by extent-id).
(defun uni-container-modified (extent-id)
   (let* (
         (container (gethash extent-id uni-extent-hash-table))
         )
      (uni-extent-modified container)
      )
   )

; Mark an extent as unmodified
(defun uni-unmodify-extent (extent)
   (set-extent-property extent 'uni-edited nil)
   )

; Mark a (container) as unmodified (by extent-id).
(defun uni-unmodify-container (extent-id)
   (let* (
         (container (gethash extent-id uni-extent-hash-table))
         )
      (uni-unmodify-extent container)
      )
   )

; Determine if an extent is modified
(defun uni-extent-modified (extent)
   (extent-property extent 'uni-edited)
   )


; We also provide uni-lock-buffer and uni-unlock-buffer to make the buffer
; read-only while Haskell is altering it.
(defun uni-lock-buffer ()
   (setq uni-buffer-lock-counter (1+ uni-buffer-lock-counter))
   )

(defun uni-unlock-buffer ()
   (setq uni-buffer-lock-counter (1- uni-buffer-lock-counter))
   )
       

; Get an extent's id
(defun uni-get-extent-id (extent)
   (extent-property extent 'uni-extent-id)
   )

; Get an extent's type
(defun uni-get-extent-type (extent)
   (extent-property extent 'uni-extent-type)
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


;
; Special functions for modifying the contents of a container.  These depend on
; the contents of point, and so need to be executed in the same command.
;
; We will also use the XEmacs delete-char function (to delete the next n
; characters) and the insert function (which inserts its argument and points
; after).

; set point to after the extent.
(defun uni-point-after-extent (extent-id)
   (let ((extent (gethash extent-id uni-extent-hash-table)))
      (goto-char (extent-end-position extent))
   ))

; add button at point and point after.
(defun uni-add-button-point (extent-id text)
   (uni-create-button-extent extent-id (point) text)
   )


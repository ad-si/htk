;; This file is loaded to define the MMiSS-init function (which the
;; Saarbrückers strangely didn't want defined in mmisstex.elc.) 
;; It also does any other necessary initialisations and defines how colours
;; are allocated to magic buttons.
(require 'tex-site)
(put 'erase-buffer 'disabled nil)
(defun MMiSS-init ()
  (setq LaTeX-mode-hook        'mmiss-mode)  

  ; The local variables uni-process and uni-half-line
  ; get stomped on by MMiSSTeX-mode so need to be reconstructed

  (let  (
        (uni-process-saved uni-process)
        (uni-half-line-saved uni-half-line)
        )
     (MMiSSTeX-mode)
     (make-local-variable 'uni-process)
     (setq uni-process uni-process-saved)
     (make-local-variable 'uni-half-line)
     (setq uni-half-line uni-half-line-saved)
     )

  (uni-initialise-extents)
  (make-local-variable 'MMiSS-frame)
  (setq MMiSS-frame (make-frame))

  (set-window-buffer (frame-root-window MMiSS-frame) (current-buffer))
  )

(defun MMiSS-delete ()
  ; The local variable MMiSS-frame will get stomped on by the kill-buffer.
  (let ((MMiSS-frame-saved MMiSS-frame))
     (kill-buffer (current-buffer))
     (delete-frame MMiSS-frame-saved)
     )
  )

; Note on colours.
; white is bad as it is just like normal text
; black or dark colours are bad as it makes the text impossible or hard to
;   read.
; these colours should be co-ordinated with those in mmiss/MMiSS.dtd.
(setq MMiSS-colours 
   (list
      (cons ?G  "orange")
      (cons ?S  "orange")

      (cons ?U  "green")
      (cons ?a  "green")
      (cons ?I  "green")
      (cons ?s  "green")
      (cons ?F  "green")
      (cons ?E  "green")

      (cons ?A  "yellow")
      (cons ?C  "yellow")
      (cons ?p  "yellow")
      (cons ?P  "yellow")
      (cons ?c  "yellow")
      (cons ?t  "yellow")
      (cons ?x  "yellow")
      (cons ?y  "yellow")
      (cons ?z  "yellow")
      (cons ?D  "yellow")

      (cons ?T  "red")

      )
   )

(setq MMiSS-extent-faces
   (mapcar
      (lambda (color-item)
         (let ((face (make-symbol "MMiSS-face")))
            (copy-face 'highlight face)
            (set-face-background face (cdr color-item))
            (cons (car color-item) face)
            )
         )
      MMiSS-colours
      )
   )

(defun MMiSS-retrieve-face (key)
   (cdr (assq key MMiSS-extent-faces))
   )
      



   

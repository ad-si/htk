;; This file is loaded to define the MMiSS-init function (which the
;; Saarbrückers strangely didn't want defined in mmisstex.elc.) 
;; It also does any other necessary initialisations.
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
  (delete-frame MMiSS-frame)
  (kill-buffer (current-buffer))
  )




   

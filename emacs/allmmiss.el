;; This file is loaded to define the MMiSS-init function (which the
;; Saarbrückers strangely didn't want defined in mmisstex.elc.) 
;; It also does any other necessary initialisations and defines how colours
;; are allocated to magic buttons.
(require 'tex-site)
(put 'erase-buffer 'disabled nil)
(defun MMiSS-init ()

;  Why in creation the Saarbrückers wanted to clobber the LaTeX mode whenever
;  you use MMiSS-LaTeX is a mystery to me.  We comment this line of theirs
;  out.  (GER, 7th October 2003)
;  (setq LaTeX-mode-hook        'mmiss-mode)  

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

; Note on colours.
; white is bad as it is just like normal text
; black or dark colours are bad as it makes the text impossible or hard to
;   read.
; these colours should be co-ordinated with those in mmiss/MMiSS.dtd.
;
; the letters correspond to XML elements.  The correspondence is defined
; by the functions fromIncludeStr(Opt) and toIncludeStr in LaTeXParser.hs.
(setq MMiSS-colours 
   (list
    ;; Note on packages: they should be orange, but there is no includePackage
    ;; element, only includePackage, hence packages are treated as groups 
    ;; with a yellow button.
      (cons ?G  "yellow") ; group
      (cons ?U  "red") ; unit
      (cons ?A  "white")  ; atom
      (cons ?E  "blue")  ; embedded

      ;;; #98ceda is light green
      ;;; #98ecb2 is light blue 

      ; This is not an MMiSS element, but used in MMiSSPreamble.hs
      (cons ?B  "blue") ; preamble
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
      



   

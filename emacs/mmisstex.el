;;;---------------------------------------------------------------
;                          MMiSSTeX:


#+ignore
;; put the following progn. body into your .emacs file to be evaluated on startup.
(progn 
  (put 'erase-buffer 'disabled nil)
test
  (defun MMiSS-init (string)
    (load-file "./mmisstex.el")
    (setq LaTeX-mode-hook        'mmiss-mode)  
    (MMiSSTeX-mode)
    (easy-menu-remove  LaTeX-mode-menu)
    (insert string)
    (insert "\n")
    (beginning-of-buffer)
    (display-buffer)
    )
  )

;; ********************************************************************************
;; 
;; README:
;; 
;; Put the previous lines defining MMiSS-init in your .emacs file.  
;;  
;; (MMiSS-init string) : initialise a buffer with where string is the content
;;                       for the buffer.
;;
;; MMiSSTeX-menu item "Request Enlarged Environment" or
;; (MMiSS-request)     : request to enlarge the visible environment. Sends to 
;;                       Haskell an event (via uni-ev) with key ENLARGE
;;                       and data the string 
;;                          [environment-id]\n[content-of-buffer]
;;                          ENLARGE
;;                       and erases the buffer such that it can get the enlarged
;;                       misstex file from haskell. 
;;                       Invariant: There is always a unique top-level environment 
;;                                  in the mmisstex file. 
;; 
;; MMiSSTeX-menu item "Commit" or
;; (MMiSS-commit)       : sends the event with key COMMIT to Haskell, and no
;;                        value.
;;
;; MMiSSTeX-menu item "Quit" or
;; (MMiSS-quit)         : sends the event with key QUIT to Haskell, and no
;;                        value.
;;
;; Steps to edit a mmisslatex document, whose content is denoted by STRING in the 
;; following, from the development manager: 
;;  1. call uni-initialize .... to initialize the mmisstex-editing mode
;;  2. call (MMiSS-init STRING) 
;;
;; ********************************************************************************

;only procedure declaration:
(defun MMiSS-request ()
  (interactive)
  (let ((id (MMiSS-top-environment-id)))
    (if id
        (progn 
          (uni-ev "ENLARGE" (concat id "\n" (buffer-string)))
          (erase-buffer)
          )
      nil)))


(defun MMiSS-top-environment-id ()
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\)\\b"))
        (startid nil)
        (endid   nil)
        (id      ""))
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward regexp nil t)
      (forward-char 1)
      (skip-chars-forward "a-zA-Z \t\n}")
      (forward-char 1)
      (setq startid (point))
      (skip-chars-forward "a-zA-Z0-9")
      (setq endid (point))
      (if (and (numberp startid) (numberp endid))
          (setq id (buffer-substring startid endid))))
    (if (equal "" id)
        (progn (error "Could not determine id of top-level MMiSS-Environment!") nil)
      id)
    ))

(defun MMiSS-commit ()
  (interactive)
  (uni-ev "COMMIT" "")
  )

(defun MMiSS-quit ()
  (interactive)
  (uni-ev "QUIT" "")
  )


(defun MMiSS-environment (environment)
"Define a MMiSSTeX environment in a (full) generic format.  
You are prompted for environment; the following
characters replies give the indicated types:
p       Package
s       Section
§       Paragraph
v       View
e       Example
x       Exercise
def     Definition
tf      TextFragment
t       Table
f       Figure
ge      GlossaryEntry
pm      Program
thy     Theory
thm     Theorem
cj      Conjecture
l       Lemma
cr      Corollary
a       Assertion
dev     Development
pf      Proof
sc      Script
pmf     ProgramFragment
c       Clause
st      Step
Any other reply is used exactly as entered."
  (interactive "MMMiSS environment type: ")
  (let ((environment-string ""))
    (if (equal environment "p")
        (setq environment-string "Package")
      (if (equal environment "s")
          (setq environment-string "Section")
        (if (equal environment "§")
            (setq environment-string "Paragraph")
          (if (equal environment "v")
              (setq environment-string "View")
            (if (equal environment "e")
                (setq environment-string "Example")
              (if (equal environment "x")
                  (setq environment-string "Exercise")
                (if (equal environment "def")
                    (setq environment-string "Definition")
                  (if (equal environment "tf")
                      (setq environment-string "TextFragment")
                    (if (equal environment "t")
                        (setq environment-string "Table")
                      (if (equal environment "f")
                          (setq environment-string "Figure")
                        (if (equal environment "ge")
                            (setq environment-string "GlossaryEntry")
                          (if (equal environment "pm")
                              (setq environment-string "Program")
                            (if (equal environment "thy")
                                (setq environment-string "Theory")
                              (if (equal environment "thm")
                                  (setq environment-string "Theorem")
                                (if (equal environment "cj")
                                    (setq environment-string "Conjecture")
                                  (if (equal environment "l")
                                      (setq environment-string "Lemma")
                                    (if (equal environment "cr")
                                        (setq environment-string "Corollary")
                                      (if (equal environment "a")
                                          (setq environment-string "Assertion")
                                        (if (equal environment "dev")
                                            (setq environment-string "Development")
                                          (if (equal environment "pf")
                                              (setq environment-string "Proof")
                                            (if (equal environment "sc")
                                                (setq environment-string "Script")
                                              (if (equal environment "pmf")
                                                  (setq environment-string "ProgramFragment")
                                                (if (equal environment "c")
                                                    (setq environment-string "Clause")
                                                  (if (equal environment "st")
                                                      (setq environment-string "Step")
                                                    (if (equal environment "be")
                                                        (setq environment-string "BibEntry")
                                                      (if (equal environment "ae")
                                                          (setq environment-string "AuthorEntry")
                                                        (setq environment-string environment)))))))))))))))))))))))))))

    (insert "\n\\begin{" 
            environment-string 
            "}\n\n\\end{" 
            environment-string 
            "}\n")
    )
  (forward-line -2)
  )

;Full reference list for shortcuts and menues:
(defun MMiSS-env-Package ()
  (interactive)
  (MMiSS-environment "p")
  )
(defun MMiSS-env-Section ()
  (interactive)
  (MMiSS-environment "s")
  )
(defun MMiSS-env-Paragraph ()
  (interactive)
  (MMiSS-environment "§")
  )
(defun MMiSS-env-View ()
  (interactive)
  (MMiSS-environment "v")
  )
(defun MMiSS-env-Example ()
  (interactive)
  (MMiSS-environment "e")
  )
(defun MMiSS-env-Exercise ()
  (interactive)
  (MMiSS-environment "x")
  )
(defun MMiSS-env-Definition ()
  (interactive)
  (MMiSS-environment "def")
  )
(defun MMiSS-env-TextFragment ()
  (interactive)
  (MMiSS-environment "tf")
  )
(defun MMiSS-env-Table ()
  (interactive)
  (MMiSS-environment "t")
  )
(defun MMiSS-env-Figure ()
  (interactive)
  (MMiSS-environment "f")
  )
(defun MMiSS-env-GlossaryEntry ()
  (interactive)
  (MMiSS-environment "ge")
  )
(defun MMiSS-env-Program ()
  (interactive)
  (MMiSS-environment "pm")
  )
(defun MMiSS-env-Theory ()
  (interactive)
  (MMiSS-environment "thy")
  )
(defun MMiSS-env-Theorem ()
  (interactive)
  (MMiSS-environment "thm")
  )
(defun MMiSS-env-Conjecture ()
  (interactive)
  (MMiSS-environment "cj")
  )
(defun MMiSS-env-Lemma ()
  (interactive)
  (MMiSS-environment "l")
  )
(defun MMiSS-env-Corollary ()
  (interactive)
  (MMiSS-environment "cr")
  )
(defun MMiSS-env-Assertion ()
  (interactive)
  (MMiSS-environment "a")
  )
(defun MMiSS-env-Development ()
  (interactive)
  (MMiSS-environment "dev")
  )
(defun MMiSS-env-Proof ()
  (interactive)
  (MMiSS-environment "pf")
  )
(defun MMiSS-env-Script ()
  (interactive)
  (MMiSS-environment "sc")
  )
(defun MMiSS-env-ProgramFragment ()
  (interactive)
  (MMiSS-environment "pmf")
  )
(defun MMiSS-env-Clause ()
  (interactive)
  (MMiSS-environment "c")
  )
(defun MMiSS-env-Step ()
  (interactive)
  (MMiSS-environment "st")
  )
(defun MMiSS-env-BibEntry ()
  (interactive)
  (MMiSS-environment "be")
  )
(defun MMiSS-env-AuthorEntry ()
  (interactive)
  (MMiSS-environment "ae")
  )
;//

; some redefinitions:
(defun MMiSS-close-environment ()
  "Create an \\end{...} to match the current environment."
  (interactive)
  (if (> (point)
         (save-excursion
           (beginning-of-line)
           (skip-chars-forward " \t")
           (point)))
      (insert "\n"))
  (insert "\\end{" (LaTeX-current-environment 1) "}")
  (LaTeX-indent-line)
  (if (not (looking-at "[ \t]*$"))
      (insert "\n")
    (let ((next-line-add-newlines t))
      (next-line 1)
      (beginning-of-line)))
  (LaTeX-indent-line))


(defun MMiSS-find-matching-end ()
  "Move point to the \\end of the current environment."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
        (level 1))
    (save-excursion
      (skip-chars-backward "a-zA-Z \t{")
      (if (bolp)
          nil
        (backward-char 1)
        (and (looking-at regexp)
             (char-equal (char-after (1+ (match-beginning 0))) ?e)
             (setq level 0))))
    (while (and (> level 0) (re-search-forward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?b);;begin
          (setq level (1+ level))
        (setq level (1- level))))
    ; (if (= level 0)
;       (search-forward "}")
;       )
    (or (= level 0)
        )
    ))

(defun MMiSS-find-matching-begin ()
  "Move point to the \\begin of the current environment."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
        (level 1))
    (skip-chars-backward "a-zA-Z \t{")
    (if (bolp)
        nil
      (backward-char 1)
      (and (looking-at regexp)
           (char-equal (char-after (1+ (match-beginning 0))) ?b)
           (setq level 0)))
    (while (and (> level 0) (re-search-backward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?e);;end
          (setq level (1+ level))
        (setq level (1- level))))
    (or (= level 0)
        )))
;//


; MMiSSTeX-mode definition:
;---------------------------


(define-derived-mode MMiSSTeX-mode
  LaTeX-mode "MMiSSTeX"
  "Major mode for MMiSSTeX.
\\{MMiSSTeX-mode-map}"
  (setq MMiSSTeX-mode-hook 'mmiss-mode))


(setq auto-mode-alist (append 
                       '( ("\\.mtex" . MMiSSTeX-mode))
                       auto-mode-alist))


(defun mmiss-mode ()
   (define-key LaTeX-mode-map "\e\em"            'MMiSS-environment)
   (define-key LaTeX-mode-map "\e\ec"            'MMiSS-close-environment)
   (define-key LaTeX-mode-map "\e\eb"            'MMiSS-find-matching-begin)
   (define-key LaTeX-mode-map "\e\ee"            'MMiSS-find-matching-end)
   (define-key LaTeX-mode-map "\e\ep"            'MMiSS-env-Package)
   (define-key LaTeX-mode-map "\e\es"            'MMiSS-env-Section)
   (define-key LaTeX-mode-map "\e\eh"            'MMiSS-env-Paragraph)
   (define-key LaTeX-mode-map "\e\ev"            'MMiSS-env-View)
   (define-key LaTeX-mode-map "\e\ex"            'MMiSS-env-Example)
   (define-key LaTeX-mode-map "\e\e\ex"          'MMiSS-env-Exercise)
   (define-key LaTeX-mode-map "\e\ed"            'MMiSS-env-Definition)
   (define-key LaTeX-mode-map "\e\e\ef"          'MMiSS-env-TextFragment)
   (define-key LaTeX-mode-map "\e\e\et"          'MMiSS-env-Table)
   (define-key LaTeX-mode-map "\e\ef"            'MMiSS-env-Figure)
   (define-key LaTeX-mode-map "\e\eg"            'MMiSS-env-GlossaryEntry)
   (define-key LaTeX-mode-map "\e\er"            'MMiSS-env-Program)
   (define-key LaTeX-mode-map "\e\ey"            'MMiSS-env-Theory)
   (define-key LaTeX-mode-map "\e\et"            'MMiSS-env-Theorem)
   (define-key LaTeX-mode-map "\e\ej"            'MMiSS-env-Conjecture)
   (define-key LaTeX-mode-map "\e\el"            'MMiSS-env-Lemma)
   (define-key LaTeX-mode-map "\e\e\ec"          'MMiSS-env-Corollary)
   (define-key LaTeX-mode-map "\e\ea"            'MMiSS-env-Assertion)
   (define-key LaTeX-mode-map "\e\e\ed"          'MMiSS-env-Development)
   (define-key LaTeX-mode-map "\e\e\er"          'MMiSS-env-Proof)
   (define-key LaTeX-mode-map "\e\e\es"          'MMiSS-env-Script)
   (define-key LaTeX-mode-map "\e\e\ep"          'MMiSS-env-ProgramFragment)
   (define-key LaTeX-mode-map "\e\e\el"          'MMiSS-env-Clause)
   (define-key LaTeX-mode-map "\e\e\ee"          'MMiSS-env-Step)
   (define-key LaTeX-mode-map "\e\e\eb"          'MMiSS-env-BibEntry)
   (define-key LaTeX-mode-map "\e\e\ea"          'MMiSS-env-AuthorEntry)
   
   (make-mmiss-menu)
)
;//
   
; Menues:
(defvar MMiSS-menu-symbol)

(defun make-mmiss-menu()
  (interactive)
  (easy-menu-define 
   MMiSS-menu-symbol
   LaTeX-mode-map
   "MMiSSTeX configuration menu" 
   (list "MMiSSTeX"
         ;;"Environments"
         (list "Insert Conceptual Environment"
               ["Package"          MMiSS-env-Package]
               ["Section"          MMiSS-env-Section]
               ["Paragraph"        MMiSS-env-Paragraph]
               ["View"             MMiSS-env-View]
               ["Example"          MMiSS-env-Example]
               ["Exercise"         MMiSS-env-Exercise]
               ["Definition"       MMiSS-env-Definition]
               ["TextFragment"     MMiSS-env-TextFragment]
               ["Table"            MMiSS-env-Table]
               ["Figure"           MMiSS-env-Figure]
               ["GlossaryEntry"    MMiSS-env-GlossaryEntry])
         
         (list "Insert Formal Environment"
               ["Program"          MMiSS-env-Program]
               ["Theory"           MMiSS-env-Theory]
               ["Theorem"          MMiSS-env-Theorem]
               ["Conjecture"       MMiSS-env-Conjecture]
               ["Lemma"            MMiSS-env-Lemma]
               ["Corollary"        MMiSS-env-Corollary]
               ["Assertion"        MMiSS-env-Assertion]
               ["Development"      MMiSS-env-Development]
               ["Proof"            MMiSS-env-Proof]
               ["Script"           MMiSS-env-Script]
               ["ProgramFragment"  MMiSS-env-ProgramFragment]
               ["Clause"           MMiSS-env-Clause]
               ["Step"             MMiSS-env-Step]
               ["BibEntry"         MMiSS-env-BibEntry]
               ["AuthorEntry"      MMiSS-env-AuthorEntry])
         ["Close Environment"            MMiSS-close-environment]
         ["Beginning of Environment"     MMiSS-find-matching-begin]
         ["End of Environment"           MMiSS-find-matching-end]
         "-"
;        ["Request Enlarged Environment" MMiSS-request]     
         ["Commit" MMiSS-commit]     
         ["Quit" MMiSS-quit]     
         ))
  (easy-menu-add MMiSS-menu-symbol LaTeX-mode-map)
  )

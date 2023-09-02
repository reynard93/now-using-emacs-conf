;;; l-org.el --- org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Common ==================================================

(use-package org
  :straight nil
	:config
	(setq
	  org-directory "~/notes/"
	  org-hide-emphasis-markers t
    org-list-allow-alphabetical t
	  org-edit-src-content-indentation 0 ;; 0 for better hugo code block
	  org-ellipsis " ▾ "
	  org-pretty-entities t
	  org-imenu-depth 4
	  org-fold-catch-invisible-edits 'smart
    org-return-follows-link t
	  org-yank-adjusted-subtrees t 
	  org-image-actual-width nil ;; don't use actual image size
	  org-log-done 'time
	  ;; org-priority-faces ;; tweak org-modern buildin symbol instead
	  ;;   '((?A :foreground "#ff6c6b" :weight bold)
	  ;;     (?B :foreground "#98be65" :weight bold)
	  ;;     (?C :foreground "#c678dd" :weight bold))
	  org-link-abbrev-alist ; This overwrites the default Doom org-link-abbrev-list
	  '(("google" . "http://www.google.com/search?q=")
		   ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
		   ("wiki" . "https://en.wikipedia.org/wiki/"))
	  org-todo-keywords ; This overwrites the default Doom org-todo-keywords
	  '((sequence
			  "TODO(t)"                ; A task that is ready to be tackled
			  "PROJ(p)"                ; A project that contains other tasks
			  "PROG(g)"                ; programming
			  "BLOG(b)"                ; Blog writing assignments
			  "WAIT(w)"                ; Something is holding up this task
			  "|" ; The pipe necessary to separate "active" states and "inactive" states
			  "DONE(d)"                       ; Task has been completed
			  "CANCELLED(c)" )))
  (push '("rust" . rust-ts) org-src-lang-modes))
(add-hook 'org-mode-hook #'org-indent-mode)

;;; org-agenda ==============================================
(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files location")

(setq org-agenda-dir "~/notes/")
(setq deft-dir  "~/notes/")
;; define the refile targets
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note))


;;; Export ==================================================
(setq org-export-with-toc t
	org-export-with-footnotes t
	org-export-coding-system 'utf-8
	org-export-headline-levels 4
	org-export-with-smart-quotes t)

;; @ latex export
;; Though the two lines below are recommended by offical, they makes error when I change latex compiler to xelatex.
;; (add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex" "xelatex" "lualatex")))
;; (add-to-list 'org-latex-packages-alist '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

;; to export inline code: src_LANG[headers]{your code}, where headers must contain ":exports code"
;; example: src_python[:exports code]{print("hello")}
(defun mk/org-export-latex-init()
  "Initialize org export latex settings. This function should be loaded after emacs init to pursue performance."
  (setq org-latex-compiler "xelatex"
    org-latex-default-class "elegantcs-en"
    ;; + image
    org-latex-image-default-width "0.8\\textwidth"
    ;; + src
    ;; by default, number=none is passed, we can use "-n"("#+BEGIN_SRC rust -n") to enable it.
    org-latex-src-block-backend 'listings
    )

  (require 'ox-latex)
  (add-to-list 'org-latex-listings-langs '(jupyter-python "Python"))
  (add-to-list 'org-latex-classes
    '("elegantcs-en" "\\documentclass[en,a4paper]{elegantcs}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
    '("elegantcs-cn" "\\documentclass[cn,a4paper]{elegantcs}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(add-hook 'after-init-hook #'mk/org-export-latex-init)

;; @ beamer (latex presentation)
(setq org-beamer-frame-level 2)
(add-hook 'after-init-hook #'(lambda () (org-beamer-mode)))

;;; Fancy face ==============================================
;; @ most of the stuffs
(use-package org-modern
	:config
	(setq
	  org-modern-star ["✿" "❀" "✜" "◉" "○" "✸" "✳" "◈" "◇"]
	  org-modern-priority
	  `((?A . ,(propertize "❗" 'face 'error))
		   (?B . ,(propertize "⚡" 'face 'warning))
		   (?C . ,(propertize "☕" 'face 'sucess)))
	  org-modern-todo-faces
	  '(("TODO" :background "#00b894" ;; green
			  :foreground "white")
		   ("PROG" :background "#e17055" ;; orange
			   :foreground "white")
		   ("PROJ" :background "#6c5ce7" ;; purple
			   :foreground "white")
		   ("BLOG" :background "#fdcb6e" ;; yellow
			   :foreground "black")
		   ("WAIT" :background "#ff7675" ;; grey
			   :foreground "white")
		   ("DONE" :background "#b2bec3" ;; grey
			   :foreground "white")
		   ("CANCELLED"  :foreground "#b2bec3")))
	(global-org-modern-mode))


;;; Facility ===============================================
;; @ tangle
;; #+auto_tangle: t
;; #+PROPERTY: header-args :tangle install.sh ;; apply to every header
(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

;; ;; @ visibility
(use-package org-appear
  :straight (:includes org) ;; prevent org-appear from installing org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t))

;; @ presentation
;; (use-package org-tree-slide
;; 	:config
;; 	(setq org-tree-slide-author "Ziqi Yang"
;; 		org-tree-slide-email "mr.ziqiyang@gmail.com"))

;;; Keybindings =============================================

(defun mk/org-local-keybinding-setup()
  ;; Cuz C-c C-c is binded by `org-ctrl-c-ctrl-c', we use M- here.
  (keymap-local-unset "C-c C-c")

  (keymap-local-set "<tab>" #'org-cycle)
  (keymap-local-set "C-S-<return>" #'org-insert-subheading)
  
  (keymap-local-set "C-c C-c" #'org-ctrl-c-ctrl-c)
  
  (keymap-local-set "C-c M-c t" #'org-todo)
  (keymap-local-set "C-c M-c T" #'org-set-tags-command)
  
  (keymap-local-set "C-c M-c c" #'org-toggle-checkbox)
  (keymap-local-set "C-c M-c i" #'org-toggle-inline-images)
  
  (keymap-local-set "C-c M-c h" #'mk/hugo/complete-tag-at-point)
  (keymap-local-set "C-c M-c b" #'org-babel-tangle)
  (keymap-local-set "C-c M-c e" #'org-export-dispatch)
  (keymap-local-set "C-c M-c p" #'org-priority))

(add-hook 'org-mode-hook 'mk/org-local-keybinding-setup)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
        ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
         "* %?\n  %i\n %U"
         :empty-lines 1)
        ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
         "* TODO [#B] %?\n  %i\n %U"
         :empty-lines 1)
        ("s" "Slipbox" entry  (file "inbox.org")
         "* %?\n")
        ("S" "Code Snippet" entry
         (file org-agenda-file-code-snippet)
         "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ("w" "work" entry (file+headline org-agenda-file-work "Work")
         "* TODO [#A] %?\n  %i\n %U"
         :empty-lines 1)
        ("x" "Web Collections" entry
         (file+headline org-agenda-file-note "Web")
         "* %U %:annotation\n\n%:initial\n\n%?")
        ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
      ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")
        ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
         "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
         :empty-lines 1)
        ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
         "* TODO [#C] %?\n  %i\n %a \n %U"
         :empty-lines 1)
        ("j" "Journal Entry"
         entry (file+datetree org-agenda-file-journal)
         "* %?"
         :empty-lines 1)))
(provide 'l-org)

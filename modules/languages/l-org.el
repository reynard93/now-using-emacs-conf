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
    org-agenda-hide-tags-regexp "."
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
        "NEXT(n)"  
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
(setq org-agenda-dir "~/notes/")
(setq deft-dir  "~/notes/")

;; define the refile targets
(setq org-agenda-file-gtd (expand-file-name "agenda.org" org-agenda-dir));;; when you know both topic and when occur
(setq org-agenda-file-projects (expand-file-name "projects.org" org-agenda-dir));;; when you know both topic and when occur
(setq org-agenda-file-notes (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-inbox (expand-file-name "inbox.org" org-agenda-dir))
;; (setq org-agenda-file-work (expand-file-name "work.org" org-agenda-dir))
;; (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
;; (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
;; (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))

(setq org-agenda-files (list org-agenda-file-gtd  org-agenda-file-notes org-agenda-file-inbox org-agenda-file-projects));;; Export
(setq org-refile-targets
  '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-agenda-custom-commands
  '(("g" "Get Things Done (GTD)"
      ((agenda ""
         ((org-agenda-skip-function
            '(org-agenda-skip-entry-if 'deadline))
           (org-deadline-warning-days 0)))
        (todo "NEXT"
          ((org-agenda-skip-function
             '(org-agenda-skip-entry-if 'deadline))
            (org-agenda-prefix-format "  %i %-12:c [%e] ")
            (org-agenda-overriding-header "\nTasks\n")))
        (agenda nil
          ((org-agenda-entry-types '(:deadline))
            (org-agenda-format-date "")
            (org-deadline-warning-days 7)
            (org-agenda-skip-function
              '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            (org-agenda-overriding-header "\nDeadlines")))
        (tags-todo "inbox"
          ((org-agenda-prefix-format "  %?-12t% s")
            (org-agenda-overriding-header "\nInbox\n")))
        (tags "CLOSED>=\"<today>\""
          ((org-agenda-overriding-header "\nCompleted today\n")))))))

(setq org-agenda-files 
  (mapcar 'file-truename 
	  (file-expand-wildcards "~/notes/*.org")))
;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
			                   (when (member (buffer-file-name) org-agenda-files) 
			                     t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
	(lambda (&rest _)
	  (gtd-save-org-buffers)))

;; (setq org-agenda-files (list org-agenda-file-gtd org-agenda-file-journal org-agenda-file-blogposts org-agenda-file-work org-agenda-file-note org-agenda-file-inbox org-agenda-file-projects));;; Export

(setq org-export-with-toc t
	org-export-with-footnotes t
	org-export-coding-system 'utf-8
	org-export-headline-levels 4
	org-export-with-smart-quotes t)
(setq org-agenda-prefix-format
  '((agenda . " %i %-12:c%?-12t% s")
     (todo   . " ")
     (tags   . " %i %-12:c")
     (search . " %i %-12:c")))

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

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
          (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

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

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

(defun my-mu4e-headers-mode-setup ()
  (define-key mu4e-headers-mode-map (kbd "C-i c") 'org-capture-mail))

(defun my-mu4e-view-mode-setup ()
  (define-key mu4e-view-mode-map (kbd "C-c c") 'org-capture-mail))

(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'mu4e-headers-mode-hook 'my-mu4e-headers-mode-setup)
(add-hook 'mu4e-view-mode-hook 'my-mu4e-view-mode-setup)

(setq org-capture-templates
  `(("i" "Inbox" entry  (file "inbox.org")
      ,(concat "* TODO %?\n"
         "/Entered on/ %U"))
     ("@" "Inbox [mu4e]" entry (file "inbox.org")
       ,(concat "* TODO Process \"%a\" %?\n"
          "/Entered on/ %U"))
     ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
       ,(concat "* %? :meeting:\n"
          "<%<%Y-%m-%d %a %H:00>>"))
     ("n" "Note" entry  (file "notes.org")
       ,(concat "* Note (%a)\n"
          "/Entered on/ %U\n" "\n" "%?"))
     ))
;; (setq org-capture-templates
;;   '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
;;       "* TODO [#B] %?\n  %i\n %U"
;;       :empty-lines 1)
;;      ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
;;        "* %?\n  %i\n %U"
;;        :empty-lines 1)
;;      ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
;;        "* TODO [#B] %?\n  %i\n %U"
;;        :empty-lines 1)
;;      ("s" "Slipbox" entry  (file "inbox.org")
;;        "* %?\n")
;;      ("S" "Code Snippet" entry
;;        (file org-agenda-file-code-snippet)
;;        "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
;;      ("w" "work" entry (file+headline org-agenda-file-work "Work")
;;        "* TODO [#A] %?\n  %i\n %U"
;;        :empty-lines 1)
;;      ("x" "Web Collections" entry
;;        (file+headline org-agenda-file-note "Web")
;;        "* %U %:annotation\n\n%:initial\n\n%?")
;;      ("p" "Protocol" entry (file+headline org-agenda-file-note "Inbox")
;;        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;;      ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Inbox")
;;        "* %? [[%:link][%:description]] \nCaptured On: %U")
;;      ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
;;        "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
;;        :empty-lines 1)
;;      ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
;;        "* TODO [#C] %?\n  %i\n %a \n %U"
;;        :empty-lines 1)
;;      ("j" "Journal Entry"
;;        entry (file+datetree org-agenda-file-journal)
;;        "* %?"
;;        :empty-lines 1)))
(provide 'l-org)

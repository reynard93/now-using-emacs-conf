;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:


;;; Lsp =====================================================
;; check eglot-server-programs to know the language programs that corresponding
;; to a certain language.
;; 
;; Notice that a language server can also execute other *optional package command*
;; like bash-language-server can execute shellcheck, you should check the optional
;; package required by the main pakcage is also installed(pacman -Qi to view)
;;
;; If you still cannot know it since the corresponding function is byte-compiled,
;; go to source code. 
;; to check the value the eglot-server-programs.

;; @ elgot ==================================================
(defun mk/add-eglot-ensure (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'eglot-ensure)))

(progn
	(setq-default eglot-events-buffer-size 0)  ;; NOTE disable log, improve performance
  ;; list of things that eglot won't change
	(customize-set-variable 'eglot-stay-out-of '(imenu))
	(customize-set-variable 'eglot-autoshutdown t) ;; automatically shutdown
	;; see outer files(like header files) as in project temporarily
	(customize-set-variable 'eglot-extend-to-xref t) 

	(mk/add-eglot-ensure '(c-mode-hook c-ts-mode-hook)) ;; c
	(mk/add-eglot-ensure '(python-mode-hook python-ts-mode-hook)) ;; python
	(mk/add-eglot-ensure '(rust-mode-hook rust-ts-mode-hook)) ;; rust
	(mk/add-eglot-ensure '(go-ts-mode-hook go-mod-ts-mode-hook)) ;; go
	(mk/add-eglot-ensure '(js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook)) ;; js/ts
	(mk/add-eglot-ensure '(html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode)) ;; web, vue(defined in l-web.el) and css
  (mk/add-eglot-ensure '(java-mode-hook java-ts-mode-hook)) ;; java (terrible)
  (mk/add-eglot-ensure '(kotlin-ts-mode-hook))
  (mk/add-eglot-ensure '(zig-mode-hook)) ;; zig

	(with-eval-after-load 'eglot
		(add-hook 'eglot-managed-mode-hook
			(lambda () ;; show diagnostics in the echo area
				;; Show flymake diagnostics first.
				(setq eldoc-documentation-functions
					(cons #'flymake-eldoc-function
						(remove #'flymake-eldoc-function eldoc-documentation-functions)))
				;; Show all eldoc feedback.
				(setq eldoc-documentation-strategy #'eldoc-documentation-compose)))

		;; to custom language server (like flags), add-to-list 'eglot-server-programs
		)

	;; corfu/orderless integration
	(setq completion-category-overrides '((eglot (styles orderless))))

	;; NOTE
	;; install markdown-mode to rich the doc
	)

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy"))

;; @ eldoc
(setq eldoc-echo-area-use-multiline-p nil)

;;; citre ===================================================
(use-package citre
  :defer t
  :init
  (require 'citre-config)
  :config
  (setq-default citre-enable-imenu-integration nil) ;; disable imenu integration
  (setq
    citre-default-create-tags-file-location 'global-cache
    citre-use-project-root-when-creating-tags t
    citre-prompt-language-for-ctags-command t
    citre-capf-substr-completion t
    citre-auto-enable-citre-mode-modes '(prog-mode))
  (add-to-list 'completion-category-overrides
    '(citre (substring basic))) ;; it seems that citre only support substring
  ;; (setq evil-lookup-func #'citre-peek) ;; mapping key "K"
  )

;;; dumb-jump ===============================================
;; (use-package dumb-jump
;;   :config
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;;   ;; (setq evil-lookup-func #'dumb-jump-quick-look)
;;   )

;;; Breadcrumb ==============================================
(use-package breadcrumb
  :straight (:type git :host github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode))

;;; Syntax Checker ==========================================
;; flymake is integrated with eglot, so we only need to enable it for emacs lisp mode
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;; Spell Checker ===========================================
;; @ ispell
(setq ispell-program-name "hunspell"
  ispell-dictionary "en_US" ;; M-: (message "%s" (ispell-valid-dictionary-list))
  ispell-alternate-dictionary (expand-file-name  "dicts/en_US-large.dic" user-emacs-directory))

;; @ dictionary
(setq dictionary-server "localhost")
(setq switch-to-buffer-obey-display-actions t)
(add-to-list 'display-buffer-alist
  '("^\\*Dictionary\\*" display-buffer-in-side-window
     (side . left)
     (window-width . 100)))

;; @ Just-in-time spell checking
;; (use-package jit-spell
;;   :straight (:type git :host github :repo "astoff/jit-spell")
;;   :hook (( text-mode org-mode) . jit-spell-mode))


;; @ jinx
(use-package jinx
  :straight (:host github :repo "minad/jinx" :files ("*.el" "*.h" "*.c"))
  :init
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  :config
  ;; (setq jinx-languages '("en_US.UTF-8" "zh_CN.UTF-8"))
  )

;;; Compile command for each mode ===========================
;; since configuration files for some mode doesn't exist, so I put it all here
(defun mk/set-compile-command ()
  "Define compile command for every mode."
  ;; to make sure buffer has corresponding file, and prevent
  ;; error when loading lisp-interaction-mode at emacs startup
  (when buffer-file-name
    (setq-local compile-command
      (let* ((base-path ;; project root when in a project; current directory when not
               (if (project-current)
                 (project-root (project-current)) ;; have problem with git submodule
                 (file-name-directory buffer-file-name)))
              (relative-file-name (file-relative-name buffer-file-name base-path))
              (relative-bare-file-name (file-name-sans-extension relative-file-name))
              (makefile-exist (file-exists-p (expand-file-name "Makefile" base-path))))
        (cond
          (makefile-exist
            "make ")
          ;; rust
          ((or (eq major-mode 'rust-mode) (eq major-mode 'rustic-mode) (eq major-mode 'rust-ts-mode)) 
            "cargo run")
          ;; emacs lisp 
          ((eq major-mode 'emacs-lisp-mode)
            (concat "emacs --debug-init --init-directory=~/.emacs.d_test/ -l "
              (project-root (project-current)) "test/init.el" " test/0.el"))
          ;; cpp
          ((or (eq major-mode 'c++-mode) (eq major-mode 'c++-ts-mode))
            (concat "make " relative-bare-file-name " && ./" relative-bare-file-name))
          ;; c
          ((or (eq major-mode 'c-mode) (eq major-mode 'c-ts-mode))
            (concat "make " relative-bare-file-name " && ./" relative-bare-file-name))
          ;; kotlin
          ((eq major-mode 'kotlin-ts-mode)
            (concat "kotlinc " relative-file-name " -include-runtime -d app.jar && kotlin ./app.jar"))
          ;; zig
          ((eq major-mode 'zig-mode)
            "zig build")
          ;; python
          ((or (eq major-mode 'python-mode) (eq major-mode 'python-ts-mode))
            (concat "python " (buffer-file-name)))
          ;; other
          (t "make "))))))

(add-hook 'prog-mode-hook #'mk/set-compile-command)

;;; Extra Project Root Markers ==============================
(setq project-vc-extra-root-markers '("Cargo.toml" ".project-root"))

(provide 'l-general)

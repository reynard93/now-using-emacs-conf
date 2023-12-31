;;; init-base.el --- Basics -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Trivil ==================================================
(require 'org-protocol)
;; Enable kill ring integration for macOS clipboard
(setq select-enable-clipboard t)
;; (setq select-enable-primary t) |# ;; this interferes with meow visit and replace working WARNING!!!
(defalias 'yes-or-no-p 'y-or-n-p)
;;; move to better-defaults custom file
(use-package exec-path-from-shell
  :straight t  ; This line ensures the package is installed using straight.el
  :init
  ;; This code will run before the package is loaded
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize))
  :config
  ;; This code will run after the package is loaded
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(use-package dired
  :straight nil
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
    "-alh")
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-guess-shell-alist-user
    '(("\\.pdf\\'" "open")
       ("\\.docx\\'" "open")
       ("\\.\\(?:djvu\\|eps\\)\\'" "open")
       ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
       ("\\.\\(?:xcf\\)\\'" "open")
       ("\\.csv\\'" "open")
       ("\\.tex\\'" "open")
       ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
         "open")
       ("\\.\\(?:mp3\\|flac\\)\\'" "open")
       ("\\.html?\\'" "open")
       ("\\.md\\'" "open")))

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  )

(use-package smartparens
  :init
  (smartparens-global-mode t)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  :config
    (sp-with-modes
        '(c++-mode objc-mode c-mode)
      (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package hungry-delete
  :init
  (global-hungry-delete-mode))

(global-auto-revert-mode t)

;; @ delete file by moving to trash
;; change the behavior of delete-file and delete-directory function
;; (setq delete-by-moving-to-trash t)
(setq make-backup-files nil) ;; dont' automatically backup files in <fileName>~ format

;;; jump back functionality (mark)
(setq mark-ring-max 6
  global-mark-ring-max 6)

(defun xah/pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_previous_position.html'
Version 2016-04-04"
  (interactive)
  (set-mark-command t))

;; @ save minibuffer history
;;; save minibuffer history
;; Persist history over Emacs restarts.
(use-package savehist
  :init
	;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1))

;;; auto revert buffer ======================================
(global-auto-revert-mode)

;;; clean directory =========================================
(use-package no-littering
	:init
	(setq
		no-littering-etc-directory (expand-file-name ".local/config/" user-emacs-directory)
		no-littering-var-directory (expand-file-name ".local/data/" user-emacs-directory))
	:config
	(require 'recentf)
	(add-to-list 'recentf-exclude no-littering-var-directory)
	(add-to-list 'recentf-exclude no-littering-etc-directory)
	(setq
	  auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	  custom-file (no-littering-expand-etc-file-name "custom.el")))

;;; workspace ===============================================
;; @ persp-mode
;; poor document, conflict with vertico-posframe when manually recover, too hard to use
;; (use-package persp-mode
;;   :hook (after-init . persp-mode)
;;   :config
;;   (setq persp-autokill-buffer-on-remove 'kill-weak
;;         persp-auto-resume-time -1 ; Don't auto-load on startup
;;         persp-auto-save-opt 1 ;; save on the emacs shutdown and only if the persp-mode active
;;         persp-reset-windows-on-nil-window-conf nil
;;         persp-nil-hidden t
;;         persp-set-last-persp-for-new-frames t
;;         ;; persp-switch-to-added-buffer nil
;;         persp-kill-foreign-buffer-behaviour 'kill
;;         persp-remove-buffers-from-nil-persp-behaviour nil)) 
;; TODO try tabspace 

;; @ switch workspace
;; (use-package eyebrowse
;;   :config 
;; 	(define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
;; 	(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;; 	(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;; 	(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;; 	(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;; 	(define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
;; 	(eyebrowse-mode t)
;; 	(setq eyebrowse-wrap-around t) ;; makes workspaces a loop
;; 	;; (setq eyebrowse-new-workspace "*dashboard*")
;;   ) ;; use *scratch* buffer (use string to provide it with custom buffer name)

;; (use-package tabspaces
;;   ;; use this next line only if you also use straight, otherwise ignore it. 
;;   :straight (:type git :host github :repo "mclear-tools/tabspaces")
;;   :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
;;   :commands (tabspaces-switch-or-create-workspace
;;               tabspaces-open-or-create-project-and-workspace)
;;   :custom
;;   (tabspaces-use-filtered-buffers-as-default t)
;;   (tabspaces-default-tab "Default")
;;   (tabspaces-remove-to-default t)
;;   (tabspaces-include-buffers '("*scratch*"))
;;   ;; sessions
;;   (tabspaces-session t)
;;   (tabspaces-session-auto-restore t))

;; @ save session(buildin)
;; (use-package desktop
;;   :custom
;;   (desktop-restore-eager 4)
;; 	(desktop-save t)
;; 	:init
;; 	(if (display-graphic-p)
;; 		;; non-daemon emacs 
;; 		(progn
;; 			(add-hook 'after-init-hook '(lambda () (desktop-save-mode t)))
;; 			;; Manually read by clicking on dashboard icon instead
;; 			;; (add-hook 'after-init-hook #'desktop-read)
;; 			)
;; 		;; emacs server
;; 		(progn
;; 			(add-hook 'server-after-make-frame-hook '(lambda () (desktop-save-mode t)))
;; 			;; we need the first emacsclient to read the session, the later opened emacsclient(the
;; 			;; first one is still alive) will not read the session since the server arleady owns the
;; 			;; session
;; 			;; Manually read by clicking on dashboard icon instead
;; 			;; (add-hook 'server-after-make-frame-hook #'desktop-read)
;; 			)
;; 		)
;; 	:config
;; 	;; Config Block makes sure this lambda function load later than desktop in kill-emacs-query-functions hook , so this lambda function is executed earlier

;; 	;; remove desktop-kill hook. Leave out the check procedure.
;; 	(remove-hook 'kill-emacs-query-functions #'desktop-kill)

;; 	(let ((save-path (expand-file-name ".local/data/desktop" user-emacs-directory)))
;; 		;; when explictly quit emacs with kill-emacs command
;; 		(add-hook 'kill-emacs-hook
;; 			`(lambda ()
;; 				 (desktop-remove)
;; 				 (desktop-save ,save-path t)))
;; 		;; when implictly quit emacs like close window
;; 		(add-hook 'kill-emacs-query-functions
;; 			`(lambda ()
;; 				 (desktop-remove) ;; make sure there is no desktop file or desktop.el will prompt you Whether override it or not
;; 				 (desktop-save ,save-path t))))) ;; save session without lock

;;; text scale change on the fly ============================
(use-package default-text-scale 
	:bind (("C--" . default-text-scale-decrease)
				  ("C-=" . default-text-scale-increase))
  :defer 1
	:hook (after-init . default-text-scale-mode))

;;; Project Utilities =======================================
;; use buildin prokect.el for project ability
;; @ enable consult to find file in project
;; (use-package consult-project-extra)
(defun mk/setup-project.el ()
  "Project.el settings."
  (setq project-switch-commands
    (remove (assoc 'project-find-regexp project-switch-commands) project-switch-commands))
  (add-to-list 'project-switch-commands '(project-find-regexp "find regexp" "G"))
  
  (add-to-list 'project-switch-commands '(consult-ripgrep "Consult rg" "r"))
  (add-to-list 'project-switch-commands '(consult-git-grep "Consult git grep" "g")))

(add-hook 'after-init-hook #'mk/setup-project.el)

;;; Window ==================================================
;; @ jump
(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?c ?n ?j ?k ?l ?i))
  (aw-minibuffer-flag t))

;; @ remember window layout for different scino
;; (use-package winner
;; 	:hook (after-init . winner-mode))

;;; Recent file =============================================
(use-package recentf
  :defer t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup 'never) ; "05:00am"
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 50)
  :config
  (add-to-list 'recentf-exclude "/elpa/.*\\'")
  (add-to-list 'recentf-exclude "/tramp.*\\'")
  (add-to-list 'recentf-exclude "/\\.git/.*\\'")
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;;; Enhance Help ============================================
;; Some symbol cannot be found(like eglot-server-programs, also the emacs
;; buildin helper, but the latter in wider cases can find, maybe same buffer
;; can find, different buffer cannot find). In this case, you should first search
;; for eglot symbol, then all the symbols related to eglot can be found at the next
;; time
(use-package helpful
	:bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;;; fold ====================================================
;; @ buildin
;; evil buildin fold throught this package
(defun mk/hs-hide-level-samrt()
  "Calling hs-hide-level based on line numbers."
  (interactive)
  (hs-minor-mode)
  (set-display-table-slot standard-display-table 
    'selective-display 
    (string-to-vector " ❡❡❡"))
  (let ((n (car (buffer-line-statistics)))
         (l3 200)
         (l2 400)
         (l1 600)
         (l0 800))
    (cond
      ((> n l0)
        (hs-hide-all)
        ;; (outline-show-only-headings)
        )
      ((> n l1) (hs-hide-all))     ;; also hide long comment
      ((> n l2) (hs-hide-level 1)) ;; show root function
      ((> n l3) (hs-hide-level 2)))))
(use-package hideshow 
	:hook ((prog-mode . mk/hs-hide-level-samrt)))


;;; save file utility =======================================
;; when change window, lose focus & idle ...
(use-package super-save
  :defer t
	:hook (after-init . super-save-mode)
  :config
  (setq
    auto-save-default nil ;; disable built-in auto-save mode
    super-save-auto-save-when-idle t
    super-save-idle-duration 0.5)) ;; 0.5s idle

;;; Zoxide ==================================================
(use-package zoxide)

;;; Search & Replce =========================================
;; @ make search and replace very easy (even for project)
;; notice: in replace: !, y, n is the keybindings to replace all, replace current and not replace current
(use-package color-rg
	:straight (:host github :repo "manateelazycat/color-rg"))

;;; Todo highlight ==========================================
(use-package hl-todo
	:hook ((after-init . global-hl-todo-mode))
	:init
	(setq hl-todo-keyword-faces
		'(("TODO"   . "#2ecc71")
			 ("FIXME"  . "#e74c3c")
			 ("DEBUG"  . "#9b59b6")
			 ("NOTE" . "#3498db")
			 ("STUB"   . "#f39c12"))))

(use-package consult-todo
  :straight (:type git :host github :repo "liuyinz/consult-todo"))

;;; Persistent Scrctch Buffer ===============================
;; (use-package persistent-scratch
;; 	:config
;; 	(persistent-scratch-setup-default))

;;; Show Key ================================================
;; for presentation usage
(use-package keycast
	:after (doom-modeline dashboard))

;;; Buffer Move (swap window) ===============================
(use-package buffer-move :defer 1)

;;; Sideline ================================================
(use-package sideline
  :defer 1
  :init
  (setq sideline-flymake-display-mode 'point)
  (setq sideline-backends-right '((sideline-flymake . up)
                                   ;; (mk/sideline-eldoc . down)
                                   ))
  :config
  (global-sideline-mode))

(use-package sideline-flymake :defer 1)

;; if run the following code on non-emacs-lisp mode using eldoc, then text-read-only error occurs
;; (defvar mk/sideline-eldoc--message "")

;; (defun mk/sideline-eldoc--set-message (str &rest args)
;;   "Extract eldoc message format STR with ARGS."
;;   (when str
;;     (message str)
;;     (setq mk/sideline-eldoc--message (apply #'format str args))))

;; (defun mk/sideline-eldoc (command)
;;   "Eldoc backend for sideline."
;;   (cl-case command
;;     (`candidates
;;       (cons :async
;;         (lambda (callback &rest _)
;;           (progn
;;             (remove-text-properties 0 (length mk/sideline-eldoc--message)
;;               '(read-only t) mk/sideline-eldoc--message)
;;             (funcall callback (split-string mk/sideline-eldoc--message "\n"))))))))

;; (setq eldoc-message-function #'mk/sideline-eldoc--set-message)

;;; Peek ====================================================
(use-package peek
  :straight (:type git :host sourcehut :repo "meow_king/peek")

  :custom
  ;; only list some mostly-want-changed settings
  (peek-overlay-window-size 11) ; lines
  ;; one line before the place found by `peek-definition' will also appear
  ;; in peek window. Note `peek-definition' is the underlying function of
  ;; `peek-xref-definition'
  (peek-definition-surrounding-above-lines 1)
  (peek-overlay-position 'above) ;; or below
  
  (peek-live-update t) ;; live update peek view of a marked region

  (peek-eldoc-message-overlay-position 2) ;; eldoc message overlay at two lines below the point

  (peek-enable-eldoc-message-integration nil) ;; disable (defaut) `eldoc-message-function' integration
  (peek-enable-eldoc-display-integration nil) ;; enable `eldoc-display-functons'  integration

  :config
  (global-peek-mode 1)

  ;; Keybindings
  ;; default keybindings in peek-mode-keymap
  (define-key peek-mode-keymap (kbd "M-n") 'peek-next-line)
  (define-key peek-mode-keymap (kbd "M-p") 'peek-prev-line)

  ;; or you can use `keymap-global-set', which is introduced in emacs 29
  ;; (global-set-key (kbd "C-x P p") #'peek-overlay-dwim)
  ;; (global-set-key (kbd "C-x P d") #'peek-xref-definition)
  ;; (global-set-key (kbd "C-x P m") #'peek-overlay-eldoc-message-toggle-stauts)
  ;; (global-set-key (kbd "C-c c d") #'eldoc)

  ;; Eldoc display setting
  ;; Besides making `peek-enable-eldoc-display-integration' to t, you may want to remove
  ;;   other eldoc display functions.
  ;; (remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)

  ;; (add-hook 'meow-insert-enter-hook 'peek-overlay-eldoc-message-enable)
  ;; (add-hook 'meow-insert-exit-hook 'peek-overlay-eldoc-message-disable)
  )

(use-package peek-collection
  :straight (:type git :host sourcehut :repo "meow_king/peek-collection"))


;;; outline minor mode ======================================
;; use TAB, ze, zE to toggle outline (evil-collection binding)
;; (defun mk/set-outline-minor-mode ()
;;   "Define compile command for every mode."
;;   (outline-minor-mode)
;;   ;; this line also set hs-hide symbol
;;   (let (comment-symbol)
;;     (cond
;;       ((or (eq major-mode 'rust-mode) (eq major-mode 'rust-ts-mode))
;;         (setq comment-symbol "//"))
;;       (t
;;         (setq comment-symbol '(syntax comment-start))))
;;     (set (make-local-variable 'outline-regexp)
;;       (eval `(rx bol ,comment-symbol (*? not-newline) (>= 10 "=") (* blank) eol)))))

;; (add-hook 'prog-mode-hook #'mk/set-outline-minor-mode)

;;; Window Configuration ==================================
(add-to-list 'display-buffer-alist
  '("\\*compilation\\*"
     (display-buffer-same-window)
     (reusable-frames . nil)))

;; Tried a lot of method, still doesn't work for man / consult-man
;; (add-to-list 'display-buffer-alist
;;   '("\\*Man.*?\\*"
;;      (display-buffer-same-window)
;;      (reusable-frames . nil)))

;;; Environment Variables ===================================
(defun mk/set-env()
  "Set environment variables for Emacs"
  (interactive)
  (setenv "GTAGSOBJDIRPREFIX" "/home/reynardlee/.cache/gtags/"))
(add-hook 'emacs-startup-hook #'mk/set-env)

;;; Terminal ====================================================================
(use-package vterm)

;;; My custom functions ===================================
(defun mk/base/copy-string-to-clipboard (str)
  ;; note this function only works in GUI version emacs
  (with-temp-buffer
    (insert str)
    (clipboard-kill-region (point-min) (point-max))))

;;; undo
(use-package vundo
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
    '(vundo-node ((t (:foreground "#808080"))))
    '(vundo-stem ((t (:foreground "#808080"))))
    '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

;;; Navigation ==================================================================
(defun mk/push-point-to-xref-marker-stack (&rest r)
  (xref-push-marker-stack (point-marker)))

(defun mk/funcs-go-back-setup()
  (dolist (func '(find-function
                   mk/better-query-replace
                   meow-beginning-of-thing
                   meow-end-of-thing
                   consult-line
                   consult-imenu
                   consult-ripgrep
                   consult-git-grep))
    (advice-add func :before 'mk/push-point-to-xref-marker-stack)))

(add-hook 'after-init-hook 'mk/funcs-go-back-setup)

;;; Hack Garbage Collector ======================================================
(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode))

;; collect gc-statistics to help developers improve gc performance
;; https://www.reddit.com/r/emacs/comments/14dej62/please_help_collecting_statistics_to_optimize/
;; (use-package emacs-gc-stats
;;   :config
;;   (setq emacs-gc-stats-remind t) ; can also be a number of days
;;   (setq emacs-gc-stats-gc-defaults 'emacs-defaults) ;; use default gc settings
;;   (emacs-gc-stats-mode +1))

(provide 'init-base)

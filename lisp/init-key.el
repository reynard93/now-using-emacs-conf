;;; init-key.el --- Key Mappings Here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; universal-argument conflict with evil mode(c-u, scroll up half screen), so we change it
(global-set-key (kbd "C-M-u") 'universal-argument)
 
;;; Key Definers and Definer Macros =========================
;; define keybindings with ease
(use-package general
  ;; :demand t ;; use mapBegin! instead
  :config
  (general-evil-setup)
 
  (general-create-definer mk/leader-def
    :prefix "SPC"
		:non-normal-prefix "S-SPC")
 
  (general-create-definer mk/local-leader-def
		:prefix "SPC m"
		:non-normal-prefix "M-SPC m"))

(defmacro mapBegin! (&rest expression)
  "Used for defining keys. Keys are defined after package 'general' load up, so we
don't need to add ':demand t' keyword to 'use-package' declearation."
  `(use-package general
    :config
    ,@expression))


;;; Evil Collection ========================================= 
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit)) ;; <escape> to act like <C-g>
  :preface
  (setq
   evil-normal-state-cursor 'box
   evil-emacs-state-cursor  'box
   evil-insert-state-cursor 'bar
   evil-visual-state-cursor 'hollow)
  :init
  (setq
   evil-want-keybinding nil ;; evil-collection required
   evil-want-C-u-scroll t
   evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; @ integrate evil to wide other mode
(use-package evil-collection
  :after evil
  :custom (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

;; @ custom key to escape
(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode t)
  (setq-default evil-escape-key-sequence "kk"))

;; @ add / change surrounding notations
(use-package evil-surround
  :after evil)

;; @ comment
(use-package evil-nerd-commenter
	:general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))

;; @ textobj
;; keybindings(incluing other useful function of this package are not here
(use-package evil-args
	:config
	;; bind evil-args text objects
	(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
	(define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; @ textobj using tree-sitter
;; TODO more hack can be here
;; TODO wait for supporting emacs29 buildin tree-sitter
;; (use-package evil-textobj-tree-sitter
;; 	:config
;; 	;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;; 	(define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; 	;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;; 	(define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;; @ navigatoin
(use-package evil-easymotion
	:config
	(evilem-default-keybindings "g"))

;;; Which-key ===============================================
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  (setq which-key-side-window-max-height 0.3)
  :config
	(which-key-mode))

;;; Main Key Mapping ========================================
(mapBegin!
 ;; @ normal map (no leader key
 (general-nmap
	 "gcc" #'evilnc-comment-or-uncomment-lines
	 "C-." #'embark-act
	 "L"   #'evil-forward-arg
	 "H"   #'evil-backward-arg
	 "C-S-o"   #'evil-jump-out-args

	 ;; TODO this is temporary, wait for news from evil-textobj-tree-sitter
	 "[f" '(treesit-beginning-of-defun :which-key "func begin")
	 "]f" '(treesit-end-of-defun :which-key "func end")

	 ;; @ text-scale via init-base/default-text-scale
	 ;; C-- and C-= to change font size

	 ;; @ fold via init-base/evil-vimish-mode
	 ;; zf(create) -> za/zc/zo(toggle/close/open) -> zd(delete)
	 )
 (general-mmap
	 "L" #'evil-forward-arg
	 "H" #'evil-backward-arg)

 ;; @ visual map (no leader key
 (general-vmap ;; visual
   "S" #'evil-surround-region
   "C-S-c" #'clipboard-kill-ring-save
	 "gc" #'evilnc-comment-or-uncomment-lines)

 ;; @ insert map (no leader key
 (general-imap ;; insert
   "C-S-v" #'clipboard-yank)

 ;; @ operation map (no leader key
 (general-omap
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit)


 ;; @ normal anad insert map (leader
 (mk/leader-def
	 :states '(normal visual)
	 :keymaps 'override
	 ":" '(eval-expression :which-key "Eval")
	 "SPC" '(execute-extended-command :which-key "M-x")

   ;; @ buffer
   "b"  '(:ignore t :which-key "Buffer & Bookmark")
   "bb" '(consult-project-buffer :which-key "switch")
   "bB" '(consult-buffer :which-key "all buffer")
	 "bd" '(evil-delete-buffer :which-key "delete")
	 "bk" '(evil-delete-buffer :which-key "delete")
	 
	 ;; @ bookmark
	 "B" '(:ignore t :which-key "Bookmark")
	 "Bb" '(bookmark-jump :which-key "switch")
	 "BB" '(bookmark-jump :which-key "switch")
	 "Bc" '(bookmark-set :which-key "create")
	 "Bd" '(bookmark-delete :which-key "delete")
	 "Bk" '(bookmark-delete :which-key "delete")
	 "BD" '(bookmark-delete :which-key "delete all")

	 ;; @ Code
	 "c" '(:ignore t :which-key "Code")
	 "ca" '(eglot-code-actions :which-key "action")
	 "cr" '(eglot-rename :which-key "rename")
	 "ci" '(eglot-code-action-organize-imports :which-key "format-buffer")
	 "cf" '(eglot-code-action-quickfix :which-key "format-buffer")
	 "cF" '(eglot-format-buffer :which-key "format-buffer")
	 "ce" '(consult-flymake :which-key "errors(b)")
	 "cd" '(xref-find-definitions :which-key "definitions")
	 "cr" '(xref-find-references :which-key "references")
	 "cD" '(eldoc-doc-buffer :which-key "doc") ;; also available as "K" in evil mode

   ;; @ file
   "f" '(:ignore t :which-key "File")
   "ff" '(find-file :which-key "find")
	 "fp" '(consult-project-extra-find :which-key "find@project")
	 "fr" '(consult-recent-file :which-key "recent")

   ;; @ help
   "h" '(:ignore t :which-key "Help")
   "hf" #'describe-function
   "hk" #'describe-key
   "ho" #'describe-symbol
   "hm" #'describe-mode
	 "hM" '(woman :which-key "man page")

	 ;; @ git
	 "g" '(:ignore t :which-key "Git")
	 "gg"  'magit-status
	 "gs"  'magit-status
   "gd"  'magit-diff-unstaged
   "gc"  'magit-branch-or-checkout
   "gl"  '(:ignore t :which-key "log")
   "glc" 'magit-log-current
   "glf" 'magit-log-buffer-file
   "gb"  'magit-branch
   "gP"  'magit-push-current
   "gp"  'magit-pull-branch
   "gf"  'magit-fetch
   "gF"  'magit-fetch-all
   "gr"  'magit-rebase

	 "o"  '(:ignore t :which-key "open")
	 "o-" '(dired-jump :which-key "dired here")
	 "o=" '(project-dired :which-key "project dired")

	 ;; @ project
	 "p" '(:ignore t :which-key "Project")
	 "pp" '(project-switch-project :which-key "switch")
	 "pe" '(flymake-show-project-diagnostics :which-key "errors(p)")
	 "pk" '(project-kill-buffers :which-key "kill buffers(p)")

	 ;; @ search
	 "s" '(:ignore t :which-key "Search")
	 "ss" '(consult-line :which-key "content")
	 "si" '(consult-imenu :which-key "imenu")
	 "sp" '(consult-ripgrep :which-key "project content")
	 "sb" '(consult-bookmark :which-key "bookmark")
	 "so" '(consult-outline :which-key "outline")

   ;; @ toggle
   "t" '(:ignore t :which-key "Toggle")
   "tw" 'whitespace-mode
   "tt" '(consult-theme :which-key "choose theme")
	 
   ;; @ window
   "w"  '(:ignore t :which-key "Window")
   "ww" #'ace-window
	 "wv" #'split-window-vertically
	 "wh" #'split-window-horizontally
	 "wq" #'evil-window-delete
	 "wd" #'evil-window-delete)
 )


(provide 'init-key)

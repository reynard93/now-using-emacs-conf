;;; Treesitter ==============================================
;; treesitter lang lib load path: /usr/local/lib and ~/.emacs.d/tree-sitter 
;; use treesit-install-language-grammar to install lang by looking into
;; treesit-language-source-alist variable
;; for manual build: https://github.com/casouri/tree-sitter-module
;; additional resources:
;; starter-guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html

;; language specific ts modes
(use-package vue-ts-mode
  :straight (:host github :repo "8uff3r/vue-ts-mode"))

(use-package mermaid-ts-mode
  :straight (:host github :repo "d4ncer/mermaid-ts-mode"))


;; currently, emacs lacks buildin rust mode, we directly enable rust-ts-mode
;; but(guess) I think though with ts support, it still lack some feature. For more feature, you
;; should use third-party rust-mode instead.

(setq treesit-language-source-alist
  '((vue "https://github.com/ikatyang/tree-sitter-vue")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (mermaid "https://github.com/monaqa/tree-sitter-mermaid")))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . mhtml-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(yaml\\|yml\\)\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(mermaid\\|mmd\\)\\'" . mermaid-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(c\\|h\\)\\'" . c-ts-mode))

;; manual(use script) build(recommend, since more language are included, but you need need to manualy hook the extra langauge. For build script, see above information), or use nf/treesit-install-all-languages for those languages defined in treesit-auto

(defun nf/treesit-install-all-languages ()
  "Install all languages specified by `treesit-language-source-alist'."
  (interactive)
  (let ((languages (mapcar 'car treesit-language-source-alist)))
    (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75))))

;; @ Automatically install and use tree-sitter major modes in Emacs 29+.
;; (use-package treesit-auto
;; 	:hook (after-init . global-treesit-auto-mode)
;;   :config
;;   (setq treesit-auto-install nil))
(use-package treesit-auto
  :demand t
  :after tree-sitter
  :hook (prog-mode . global-treesit-auto-mode))

(use-package ts-fold
  :straight (:host github :repo "emacs-tree-sitter/ts-fold")
  :hook (prog-mode . ts-fold-mode))

(provide 'treesitter)

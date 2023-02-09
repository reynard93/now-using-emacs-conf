;;; editor.el --- Editor Settings -*- lexical-binding: t -*-
;;; Commentary:
;; Not Incluing KeyBindings
;;; Code:

(setq-default tab-width 2
							evil-shift-width tab-width
							scroll-margin 14
							select-enable-clipboard nil ;; make register indepentent from clipboard
							)

;; @ remember cursor position
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))



;;; Paren ===================================================
;; @ color for all 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; @ according to point position
(use-package highlight-parentheses
	:hook ((prog-mode . highlight-parentheses-mode))
	:config
	(setq highlight-parentheses-attributes '((:box
																						( :line-width (1 . -1)
																							:color ,(face-attribute 'shadow :foreground))))))

(use-package paren
	:custom
	(show-paren-when-point-inside-paren t))

;; @ use smartparens instead
;; (use-package elec-pair
;;   :hook ((prog-mode org-mode) . electric-pair-mode)
;;   :config
;;   (setq electric-pair-pairs '( ; make electric-pair-mode work on more brackets.
;;                               (?\{ . ?\})
;;                               (?\[ . ?\])
;;                               (?\< . ?\>)
;;                               )))

(use-package smartparens
  :hook ((prog-mode org-mode) . smartparens-mode)
	:config
	;; (sp-pair "<#" "#>") ;; example
	(sp-pair "<" ">")
	(sp-pair "$" "$"))

;;; Indentation =============================================
(use-package aggressive-indent ;; hello
	:hook ((prog-mode . aggressive-indent-mode)))

;;; Focus ===================================================
;; focus mode, dim other text color
;; (use-package focus)

(provide 'editor)

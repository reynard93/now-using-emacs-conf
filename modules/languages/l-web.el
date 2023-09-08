;;; l-web.el --- Web Development -*- lexical-binding: t -*-
;;; Commentary:
;; 
;;; Code:

;;; html 
;; in insert mode: C-j or C-<return> to expand
(use-package emmet-mode
	:hook ((web-mode . emmet-mode)))

;;; Css =====================================================
(use-package rainbow-mode
	:hook ( ((mhtml-mode html-mode css-mode web-mode) . rainbow-mode) )) ;; TODO more specific mode

;;; Vue =====================================================
;; @ Custom vue mode based on web-mode
;; config for web mode
(defun my-web-mode-indent-setup ()
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-heredoc-fontification t)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0) ; web-mode, vue sfc no padding in the script section
  (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file, default is 4
  (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)   ; web-mode, js code in html file
  )

(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (my-web-mode-indent-setup)
  :custom-face
  (web-mode-current-element-highlight-face
    ((t (:background "#d9dbd7" :foreground "#2d3428"))))
  ;; light color for highlighting the current HTML element's column
  (web-mode-current-column-highlight-face
    ((t (:background "#d9dbd7"))))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  )

(defun mk/setup-web-mode-for-emacs-client ()
  "Setup some values of web mode for emacs client.
Due to web-mode bug for emacs client, some customizable values need to be set after emacs client reload.  `display-graphic-p'."
  (if (display-graphic-p)
    (setq web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-indentation t
      web-mode-enable-auto-opening t
      web-mode-enable-auto-quoting t
      web-mode-enable-css-colorization t)))

(add-hook 'server-after-make-frame-hook #'mk/setup-web-mode-for-emacs-client)

(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; eglot for vue-mode
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions (
                                                                                                             :typescript (:tsdk
                                                                                                                           "/Users/reylee/Library/Application Support/fnm/node-versions/v18.9.1/installation/lib/node_modules/typescript/lib")
                                                                                                             :languageFeatures (:completion
                                                                                                                                 (:defaultTagNameCase "both"
                                                                                                                                   :defaultAttrNameCase "kebabCase"
                                                                                                                                   :getDocumentNameCasesRequest nil
                                                                                                                                   :getDocumentSelectionRequest nil)
                                                                                                                                 :diagnostics
                                                                                                                                 (:getDocumentVersionRequest nil))
                                                                                                             :documentFeatures (:documentFormatting
                                                                                                                                 (:defaultPrintWidth 100
                                                                                                                                   :getDocumentPrintWidthRequest nil)
                                                                                                                                 :documentSymbol t
                                                                                                                                 :documentColor t))))))

;;; Trivial =================================================
(defun mk/live-web-start()
  "Start live web server process using browser-sync."
  (interactive)
  (condition-case nil	(delete-process "live-web")	(error nil))
  (if (project-current) ;;; start browser-sync in the project root if in a project
    (start-process-shell-command "live-web"
      "*my-buffer*"
      (concat "browser-sync start --server " (project-root (project-current)) " --files '*.html,*.css,*.js,**/*.html,**/*.css,**/*.js'"))
    (start-process-shell-command "live-web"
      "*my-buffer*"
      "browser-sync start --server --files '*.html,*.css,*.js,**/*.html,**/*.css,**/*.js'"))
  (message "live web start"))

(defun mk/live-web-kill()
  "End live web server process."
  (interactive)
  (condition-case nil
      (delete-process "live-web")
    (error nil))
  (message "live web killed"))

(defun mk/live-web-toggle()
  "Toggle live web"
  (interactive)
  (if (get-process "live-web")
    (mk/live-web-kill)
    (mk/live-web-start)))

(defun mk/web-local-keybinding-setup()
  (keymap-local-set "C-c C-c s" #'mk/live-web-start)
  (keymap-local-set "C-c C-c t" #'mk/live-web-toggle)
  (keymap-local-set "C-c C-c k" #'mk/live-web-kill))

(defun mk/add-web-local-map-hook (hook-list)
	(dolist (mode hook-list)
		(add-hook mode #'mk/web-local-keybinding-setup)))

(mk/add-web-local-map-hook '(js-mode-hook js-ts-mode-hook tsx-ts-mode-hook typescript-ts-mode-hook typescript-mode-hook))
(mk/add-web-local-map-hook '(html-mode-hook mhtml-mode-hook vue-mode-hook css-mode-hook css-ts-mode)) ;; web, vue(defined in l-web.el) and css


(use-package jest
  :hook ((vue-mode . jest-minor-mode)(js-ts-mode . jest-minor-mode)))
(provide 'l-web)

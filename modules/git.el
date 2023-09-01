;;; git.el --- git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Magit ===================================================
;; use C-j and C-k to navigate instead
;; when meet key conflicts, please refer to evil-collection
;; https://github.com/emacs-evil/evil-collection
;; FIXME it seems that magit has performance issue
;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
;; applying performance enhancements from the blog article
;; Warning: This reduces the information Magit shows you. The status buffer will be blank if you have no changes. I find this tradeoff to be worth it.
(use-package magit
  :bind ("C-c g" . magit-status)
  :init
  (use-package with-editor :ensure t)
  ;; Have magit-status go full screen and quit to previous
  ;; configuration.  Taken from
  ;; http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
  ;; and http://irreal.org/blog/?p=2253
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-quit-window (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
	:config
	(setq magit-status-buffer-switch-function #'switch-to-buffer)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
	:custom
  (magit-git-executable "/usr/bin/git");; hardcoding the path is said to make diff on macOS
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; @ Forge
;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
(use-package forge
  :after magit)

(use-package magit-todos
  :defer t)

;;; Diff-hl =================================================
(use-package diff-hl
	:hook ((after-init . global-diff-hl-mode))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.

	;; make sure it works in daemon mode
	(add-hook 'server-after-make-frame-hook
		#'(lambda () (unless (display-graphic-p)
									 (diff-hl-margin-mode))))
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'git)

;;; mail.el --- Email Configuartion -*- lexical-binding: t -*-
;;; Commentary:
;; I have difficulty using email itself to send email by Gmail SMTP server (whether using proxy setting via `proxy-http-toggle' command or proxcychains external command-line utility).
;; set firefox as default mail client:
;;   xdg-settings set default-url-scheme-handler mailto firefox.desktop
;; then set gmail website as default mailto associated application in firefox in 'setting'
;;; Code:
(defun mk/setup-mail()
  "Setup Email Configuration."
  ;; (setq message-send-mail-function 'message-send-mail-with-mailclient)
  (setq send-mail-function 'sendmail-send-it
    message-send-mail-function 'sendmail-send-it
    sendmail-program (executable-find "msmtp")
    message-sendmail-envelope-from 'header
    ;; mail-specify-envelope-from t
    ;; message-sendmail-envelope-from 'header
    message-auto-save-directory "~/mail_save"
    message-default-mail-headers "Cc: \nBcc: \n"))

(add-hook 'after-init-hook #'mk/setup-mail)

(defun mk/mu4e-main-local-keybinding-setup()
  (keymap-local-set "J" #'mu4e-search-maildir))
(defun mk/mu4e-view-local-keybinding-setup()
  ;; SPC-m r
  (keymap-local-set "M-r" #'mu4e-compose-reply))

(use-package mu4e
  :load-path  "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  ;; installing 'mu' though 'pacman -S mu' will automatically add package 'mu4e' into emacs site-package
  :straight nil
  :ensure nil
  :demand t
  :hook ((mu4e-main-mode . mk/mu4e-main-local-keybinding-setup)
          (mu4e-view-mode . mk/mu4e-view-local-keybinding-setup))
  :config
  (setq
    mu4e-mu-binary (executable-find "mu")
    mu4e-confirm-quit nil
    mu4e-maildir "~/.maildir"
    ;; Lists frequently used mail directories for quick access.
    mu4e-maildir-list '("~/mail")
    mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
    mu4e-update-interval 300
    mu4e-attachment-dir "~/Desktop"
    ;; rename files when moving - needed for mbsync:
    mu4e-change-filenames-when-moving t
    mu4e-user-mail-address-list '("yylee.rey@gmail.com")
    mu4e-use-fancy-chars t
    ;; setup shortcut with 'i' path is also relative to mu4e-maildir.
    mu4e-maildir-shortcuts
    '(("/gmail/INBOX" . ?g)
       ("/gmail/[Gmail]/Sent Mail" . ?G))
    mu4e-contexts
    `(,(make-mu4e-context
         :name "gmail"
         :enter-func
         (lambda () (mu4e-message "Enter yylee.rey@gmail.com context"))
         :leave-func
         (lambda () (mu4e-message "Leave yylee.rey@gmail.com context"))
         :match-func
         (lambda (msg)
           (when msg
             (mu4e-message-contact-field-matches msg
               :to "dummy@gmail.com")))
         :vars '((user-mail-address . "yylee.rey@gmail.com")
                  (user-full-name . "Reynard Lee")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-refile-folder . "/gmail/Archive")
                  (mu4e-sent-folder . "/gmail/Sent")
                  (mu4e-trash-folder . "/gmail/Trash"))))
    mu4e-context-policy 'pick-first ;; start with the first (default) context;
    mu4e-compose-context-policy 'ask))

;; for sending mails
(require 'smtpmail)
(provide 'mail)

;;; ai.el --- AI Powered Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst openai-api-key-path (expand-file-name "openai-api-key.txt" user-emacs-directory))
;; (defvar openai-api-key)

;; (defun mk/get-openai-api-key ()
;;   "Get openai-api-key from ~/.emacs.d/openai-api-key.txt"
;;   (interactive)
;;   (if (bound-and-true-p openai-api-key)
;;     openai-api-key
;;     (let ((key (with-temp-buffer
;;                  (insert-file-contents openai-api-key-path)
;;                  (substring (buffer-string) 0 -1))))
;;       (setq openai-api-key key)
;;       key)))

;; (use-package gptel
;;   :defer t
;;   :straight (:host github :repo "karthink/gptel" :files ("*.el"))
;;   :config
;;   (setq gptel-api-key #'mk/get-openai-api-key
;;     gptel-use-curl nil))

(use-package mind-wave
  :straight (:host github :repo "manateelazycat/mind-wave" :files ("*.el" "*.py"))
  :config
  (setq mind-wave-api-key-path openai-api-key-path
    mind-wave-api-base "https://openai-proxy.animer.live/v1"))

(use-package khoj
  :after org
  :straight (khoj :type git :host github :repo "khoj-ai/khoj" :files (:defaults "src/interface/emacs/khoj.el"))
  ;; not mentioned in the online quick start, but this will prevent the emacs package
  ;; downloading and configuring a separate khoj instance and will use already running
  ;; one instead.
  :config
  (setq khoj-auto-setup nil))

(use-package openai
  :straight (openai :type git :host github :repo "emacs-openai/openai"))

(use-package chatgpt
  :straight (chatgpt :type git :host github :repo "emacs-openai/chatgpt")
  :requires openai
  :config
  (setq openai-key #'openai-key-auth-source))

(provide 'ai)

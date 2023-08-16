;;; init-syntaxcheck.el -*- lexical-binding: t no-byte-compile: t -*-

;;; flymake-eslint, prettier, whitespace from https://github.com/angrybacon/dotemacs/blob/master/lisp/use-lint.el
(use-package flymake-eslint
  :functions flymake-eslint-enable
  :preface
  (defun me/flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                 (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

(provide 'init-syntaxcheck)

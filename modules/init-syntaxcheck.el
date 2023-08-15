;;; init-syntaxcheck.el -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2021-2023 zilongshanren

;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/emacs.d


;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

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

;;;; Prettier
(use-package prettier
  :init
  (add-to-list 'safe-local-eval-forms '(prettier-mode)))

;;;; Whitespaces
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-display-mappings '((tab-mark ?\t [?› ?\t])))
  (whitespace-global-modes '(prog-mode text-mode))
  (whitespace-line-column nil)
  (whitespace-style '(empty face lines-tail tab-mark tabs trailing))
  :hook
  (after-init . global-whitespace-mode))

(provide 'init-syntaxcheck)

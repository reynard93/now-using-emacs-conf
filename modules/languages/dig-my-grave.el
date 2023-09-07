;;; dig-my-grave.el --- jeremyf's org code block     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Reynard Lee

;; Author: Reynard Lee <yylee.rey@gmail.com>
;; Keywords:
(require 'org)

(defvar dig-my-grave/templates-alist/org-mode
  '(("Bash" . "#+begin_src bash :results scalar replace :exports both :tangle yes\n#+end_src")
     ("Blockquote" . tempel-insert-blockquote_block)
     ("Details and Summary" . "#+begin_details\n#+begin_summary\n\n#+end_summary\n#+end_details")
     ("Emacs Lisp" . "#+begin_src emacs-lisp\n#+end_src")
     ("Org Structure" . org-insert-structure-template)
     ("Plant UML" . "#+begin_src plantuml\n@startuml\n!theme amiga\n\n@enduml\n#+end_src")
     ("Ruby" . "#+begin_src ruby\n#+end_src")
     ("Rust" . "#+begin_src rust\n#+end_src")
     ("Update" . tempel-insert-update_block))
  "A list of `cons' cells with `car' as the label and `cdr' as
 the value that we'll insert.  Used as the collection for the
 `dig-my-grave' `completing-read'.")

(define-key org-mode-map (kbd "`") #'dig-my-grave)
(defun dig-my-grave ()
  "Three consecutive graves (e.g. “`”) at the start of the line prompts for
 inserting content.  See `dig-my-grave/templates-alist/org-mode'."
  (interactive)
  (if (or (and (> (point) 3)
            (string= (buffer-substring-no-properties
                       (- (point) 3) (point)) "\n``"))
        ;; Account for starting on the first line
        (and (= (point) 3)
          (string= (buffer-substring-no-properties
                     (- (point) 2) (point)) "``")))
    ;; We have just hit our third backtick at the beginning of the line.
    (progn
      (delete-char -2)
      ;; I use the alist-get pattern a lot...perhaps a function?
      (let ((value (alist-get (completing-read "Special Content: "
                                  dig-my-grave/templates-alist/org-mode nil t)
                     dig-my-grave/templates-alist/org-mode nil nil #'string=)))
        (cond
          ;; Let's assume that we're dealing with registered org blocks.
          ((stringp value)
            (insert value) (forward-line -1) (org-edit-special))
          ;; Trust the function
          ((commandp value) (call-interactively value))
          ((functionp value) (funcall value))
          ((ad-lambda-p) (funcall value))
          ;; Time for a pull request
          (t (error "Unprocessable value %s for #'dig-my-grave" value)))))
    (setq last-command-event ?`)
    (call-interactively #'org-self-insert-command)))

(provide 'dig-my-grave)
;;; dig-my-grave.el ends here

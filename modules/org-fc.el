(use-package hydra)
(use-package org-fc
  :straight
  (:type git :host sourcehut :repo "l3kn/org-fc"
    :files (:defaults "awk" "demo.org"))
  :custom
  (org-fc-directories '("~/notes/"))
  :config
  (require 'org-fc-hydra))

(provide 'org-fc)

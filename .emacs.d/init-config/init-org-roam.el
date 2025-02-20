(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-dailies-directory "daily/")
  ;; If you're usingaaz vertical completion framework, you might want a more informative completion interface
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %<%H:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-completion-everywhere t)
  ;:bind (("C-c n l" . org-roam-buffer-toggle)
         ;("C-c n f" . org-roam-node-find)
         ;("C-c n g" . org-roam-graph)
         ;("C-c n i" . org-roam-node-insert)
         ;("C-c n c" . org-roam-capture)
         ;:map org-mode-map
         ;("C-M-i" . completion-at-point)
         ;:map org-roam-dailies-map
         ;("Y" . org-roam-dailies-capture-yesterday)
         ;("T" . org-roam-dailies-capture-tomorrow))
         ;; Dailies
         ;("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(setq
  org-hugo-base-dir "~/second-brain/"
  org-hugo-section "notes")

(provide 'init-org-roam)

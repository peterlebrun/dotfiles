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
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#-title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain (file "~/org/roam/_templates/book-note-template.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n*Tasks\n\n** TODO Add initial tasks\n\n*Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
         ;; Dailies
         ;("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))


(provide 'init-org-roam)

;;; init-org.el --- Org mode configuration
;;; Commentary:
;;; Code:
(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-log-done t)
  (setq org-capture-templates
	'(("a"  "My TODO task format." entry
	   (file "todo.org")
	   "* TODO %?")))
  )

(evil-leader/set-key-for-mode 'org-mode
  "d" 'org-schedule
  "s" 'org-sort-entries
  "t" 'org-todo)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(provide 'init-org)
;;; init-org.el ends here

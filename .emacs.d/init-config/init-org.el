;;; init-org.el --- Org mode configuration
;;; Commentary:
;;; Code:
(defun pbl--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	(pri-value (* 1000 (- org-lowest-priority priority)))
	(pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
	subtree-end
      nil)))

(use-package org
  :ensure t
  :config
  (setq org-agenda-files '("~/Dropbox/org"))
  (setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  (setq org-directory "~/Dropbox/org")
  (setq org-archive-location "~/Dropbox/org/archive.org")
  (setq org-log-done 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-custom-commands
	'(("c" "Simple agenda view"
	   ((tags "PRIORITY=\"A\""
		  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		   (org-agenda-overriding-header "High-priority unfinished tasks:")))
	    (agenda "")
	    (alltodo ""
		     ((org-agenda-skip-function
		       '(or (pbl--org-skip-subtree-if-priority ?A)
			    (org-agenda-skip-if nil '(scheduled deadline))))))))))
  (setq org-capture-templates
	'(("a"  "My TODO task format." entry
	   (file "todo.org")
	   "* TODO\s%?
SCHEDULED: %t")))
  )

(defun pbl--org-sort-entries ()
  "Sort entries in todo order"
  (interactive)
  (org-sort-entries nil ?o))

(evil-leader/set-key-for-mode 'org-mode
  "d" 'org-deadline
  "s" 'org-schedule
  "o" 'pbl--org-sort-entries
  "t" 'org-todo)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(provide 'init-org)
;;; init-org.el ends here

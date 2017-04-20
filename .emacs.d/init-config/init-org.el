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

(defun pbl--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
	subtree-end
      nil)))

(defun pbl--org-agenda-next-header ()
  "Jump to next header in an agenda series."
  (interactive)
  (pbl--org-agenda-goto-header))

(defun pbl--org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (pbl--org-agenda-goto-header t))

;; This was written by @aaronbieber
(defun pbl--org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forward or BACKWARDS."
  (let ((pos (save-excursion
	       (goto-char (if backwards
			      (line-beginning-position)
			    (line-end-position)))
	       (let* ((find-func (if backwards
                               'previous-single-property-change
                             'next-single-property-change))
                (end-func (if backwards 'max 'min))
                (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                   (funcall find-func (point) 'org-agenda-date-header)))
                (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                (prop-pos (if all-pos (apply end-func all-pos) nil)))
           prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

;; This was written by @aaronbieber
(defun pbl--org-agenda-capture (&optional vanilla)
  "Capture a task in agenda mode, using the date at point.

If VANILLA is non-nil, run the standard `org-capture'."
  (interactive "P")
  (if vanilla
      (org-capture)
    (let ((org-overriding-default-time (org-get-cursor-date)))
      (org-capture nil "a"))))

(use-package org
  :ensure t
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-all-today t)
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
	    (agenda "" ((org-agenda-ndays 3))))
	   ((org-agenda-compact-blocks t)))))
  (setq org-capture-templates
	'(("a"  "A TODO task." entry
	   (file "todo.org")
	   "* TODO %?")
	  ("h" "A new habit." entry
	   (file "todo.org")
	   "* TODO %?
:PROPERTIES:
:STYLE: habit
:LAST_REPEAT:
:END:"
	   )))

  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (define-key org-agenda-mode-map "j" 'org-agenda-next-item)
	      (define-key org-agenda-mode-map "k" 'org-agenda-previous-item)
	      (define-key org-agenda-mode-map "J" 'pbl--org-agenda-next-header)
	      (define-key org-agenda-mode-map "K" 'pbl--org-agenda-previous-header)
	      (define-key org-agenda-mode-map "c" 'pbl--org-agenda-capture)
	      ))
  )

(defun pbl--org-sort-entries ()
  "Sort entries in todo order"
  (interactive)
  (org-sort-entries nil ?o))

(evil-leader/set-key-for-mode 'org-mode
  "SPC" 'org-todo
  "g" 'org-deadline
  "o" 'pbl--org-sort-entries
  "s" 'org-schedule
  "t" 'org-set-tags
  "y" 'org-priority-up
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("•")))

(provide 'init-org)
;;; init-org.el ends here

	    ;;;(alltodo ""
		  ;;;   ((org-agenda-skip-function '(or (pbl--org-skip-subtree-if-habit)
			;;;			     (pbl--org-skip-subtree-if-priority ?A)
			;;;			     (org-agenda-skip-if nil '(scheduled deadline))))
		  ;;;    (org-agenda-overriding-header "ALL normal priority tasks:"))))

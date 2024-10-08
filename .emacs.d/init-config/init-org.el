(pbl--profile "global set key")
(global-set-key (kbd "C-c a") 'org-agenda)
(pbl--profile "global set key")

(pbl--profile "require org")
(require 'org)
(pbl--profile "require org")

(pbl--profile "set face attributes")
; Adjusting face attributes requires that org-faces have loaded (these come from `(use-package org ...)
;; Not sure if I should be setting these but eff it, YOLO
(set-face-attribute 'org-agenda-structure nil :foreground "LightGray" :weight 'bold :underline t)
(set-face-attribute 'org-agenda-date nil :foreground "DarkGray" :weight 'ultra-light :underline nil)
(set-face-attribute 'org-agenda-date-weekend nil :foreground "DarkGray" :weight 'ultra-light)
(set-face-attribute 'org-agenda-date-today nil :foreground "DarkGray" :weight 'ultra-light :slant 'normal)
(set-face-attribute 'org-scheduled nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-scheduled-today nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-todo nil :foreground "LightGreen" :weight 'ultra-light)
(set-face-attribute 'org-done nil :foreground "#8C5353" :weight 'ultra-light)
(set-face-attribute 'org-upcoming-deadline nil :foreground "white")
(set-face-attribute 'org-warning nil :foreground "white")
(pbl--profile "set face attributes")

(pbl--profile "set org directory & archive")
;;file to save todo items
(setq org-directory "~/org")
(defun pbl--get-org-file (filename)
  "Syntactic sugar to return full org file path"
  (concat org-directory "/" filename))

(setq org-archive-location
      (pbl--get-org-file "archive.org::"))
(pbl--profile "set org directory & archive")

(pbl--profile "set values")
;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)
(setq org-catch-invisible-edits t)
(setq org-M-RET-may-split-line nil)
(setq org-return-follows-link t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-time-leading-zero t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-window-setup 'current-window)
(setq org-export-initial-scope 'subtree)
(setq org-default-notes-file
      (pbl--get-org-file "notes.org"))
(setq org-stuck-projects '("+active+LEVEL=2/-COMPLETE" ("TODO")))
(setq org-agenda-use-time-grid nil) ; I don't find this useful
(setq org-tags-exclude-from-inheritance '("next" "st"))
(setq org-agenda-use-tag-inheritance nil)
;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; don't give a warning color to tasks with impending deadlines
;; if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
(setq org-agenda-inhibit-startup t)
(setq org-agenda-dim-blocked-tasks nil)

(setq org-enforce-todo-dependencies t)

(setq pbl-org-agenda-project-name-size 21)
(setq pbl-org-agenda-sparkline-size 15)
(setq pbl-org-agenda-sparkline-start "|")
(setq pbl-org-agenda-sparkline-end "|")
(setq pbl-org-agenda-sparkline-body ?·) ;used as a character
(setq pbl-pad-val-size 3)
(pbl--profile "set values")

(pbl--profile "define functions")
;; Note 20190916: This would make a good blog post
(defun pbl-format-project-prefix ()
  "Format project prefix to show parent heading"
  (let* ((project-name (cadr (org-get-outline-path)))
	 (sparkline (pbl-get-sparkline (pbl-get-stats-cookie)))
	 (project-name-size pbl-org-agenda-project-name-size)
	 (project-name-length (length project-name))
	 (num-spaces (- project-name-size project-name-length)))
    (concat
     (if (< num-spaces 0)
         (substring project-name 0 project-name-size)
       (concat project-name (make-string num-spaces ?\ )))
     " "
     sparkline
     " ")))

(defun pbl-get-stats-cookie ()
  "Get stats cookie of parent heading of current todo task"
  (save-excursion
    (let* ((header (progn
                     (org-up-heading-safe)
                     (thing-at-point 'line)))
           (split-header (split-string header))
           (header-length (safe-length split-header))
           (cookie (nth (- header-length 2) split-header)))
      (substring-no-properties cookie))))

(defun pbl-pad-val (val total-size)
  "Pad val up to TOTAL-SIZE."
  (let* ((val-string (if (numberp val) (number-to-string val) val))
         (num-spaces (max 0 (- total-size (length val-string))))
         (spaces (make-string num-spaces ?\ )))
      (concat spaces val-string)))

;; Note 20191205: This would make a great blog post
(defun pbl-get-sparkline (stats)
  "Display sparkline showing progress from STATS, or nothing if STATS is not well-formed"
  (if (char-equal (string-to-char (substring stats 0 1)) ?\[)
      (let* ((stats-int (replace-regexp-in-string "\\[\\|\\]\\|%" "" stats))
             (vals (split-string stats-int "/"))
             (numerator (string-to-number (car vals)))
             (denominator (string-to-number (cadr vals)))
             (stats-float (/ (float numerator) denominator))
             (sparkline-size pbl-org-agenda-sparkline-size)
             (num-bars (truncate (* stats-float sparkline-size)))
             (num-spaces (- sparkline-size num-bars)))
        (concat
         (pbl-pad-val numerator pbl-pad-val-size)
         pbl-org-agenda-sparkline-start
         (make-string num-bars pbl-org-agenda-sparkline-body)
         (make-string num-spaces ?\ )
         pbl-org-agenda-sparkline-end
         (pbl-pad-val denominator pbl-pad-val-size)))
    ""))

(setq org-confirm-elisp-link-not-regexp "org-capture.*")

(defun org-summary-todo (n-done n-not-done)
  "Switch project to COMPLETE when all subtasks are done, to IN PROGRESS otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (if (= n-done 0) (org-todo "NOT STARTED")
      (if (> n-not-done 0) (org-todo "IN PROGRESS")
        (progn
          (org-todo "COMPLETE")
          (org-set-tags-to (seq-remove
                            (lambda (e) (string= e "active"))
                            (org-get-tags))))))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(defun pbl-get-parent-header ()
  (save-excursion
    (org-up-heading-safe)
    (substring-no-properties
     (org-get-heading t t t t))))

; @TODO This really needs to be improved but it's a good start
(defun pbl-org-state-change-hook ()
  "When in a project, remove 'next' tag from current task and add it to next task"
  (when (string= org-state "DONE")
  (let ((tags (org-get-tags))
        (current-project (pbl-get-parent-header)))
    (when (member "next" tags)
      ; Remove "next" tag from current item
      (org-set-tags-to ())

      ; Add "next" tag to next item
      ; Move forward until we get to the next heading
      (forward-line)
      (while (not (org-at-heading-p)) (forward-line))
      (if (string= current-project (pbl-get-parent-header))
          (org-set-tags-to '("next")))))))

(add-hook 'org-after-todo-state-change-hook 'pbl-org-state-change-hook)

; Open question 20190801: if I have the same state in both subsequences,
; will that cause problems? Motivation: I was getting issues where
; it seemed like I was jumping between sequences when both had a state
; called "IN PROGRESS"
; sequence 1: task states
; sequence 2: project states
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")
        (sequence "NOT STARTED" "IN PROGRESS(!)" "|" "COMPLETE")))

; Just kinda funsies
(add-to-list 'org-structure-template-alist (list "p" ":PROPERTIES:\n?\n:END:"))
(add-to-list 'org-structure-template-alist
             (list "sp" (concat "*** NOT STARTED ?\n"
                                "    :PROPERTIES:\n"
                                "    :ORDERED:  t\n"
                                "    :END:")))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("h" "habit" entry (file+headline "~/org/habit.org" "habits")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:STYLE: habit\n:END:")
        ("t" "task" entry (file+headline ("~/org/task.org") "tasks")
         "* TODO %?\nSCHEDULED: %t")
        ("j" "daily-goals" entry (file+headline "~/org/goal.org" "goals")
         "* TODO %?                                             :st:dailygoal:
DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
        ("k" "weekly-goals" entry (file+headline "~/org/goal.org" "goals")
         "* TODO %?                                             :st:weeklygoal:
DEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1w\"))")
        ("p" "project" entry (file+headline "~/org/project.org" "projects")
         "* NOT STARTED %?\n** TODO :next:")
        ;; Daily captures below
        ("f" "freewrite" entry (file+olp+datetree "~/org/pensieve.org" "pensieve")
         "* %U freewriting:%?\n")
        ("a" "morning-writing" entry (file+olp+datetree "/org/pensieve.org" "pensieve")
         (file "/opt/dropbox/org/templates/morning-writing.org"))
        ("d" "daily-review" entry (file+olp+datetree "~/org/pensieve.org" "pensieve")
         (file "/opt/dropbox/org/templates/daily-review.org"))
        ("w" "weekly-review" entry (file+olp+datetree "~/org/pensieve.org" "pensieve")
         (file "/opt/dropbox/org/templates/weekly-review.org"))
        ("r" "waiting-on" entry (file+headline "~/org/task.org" "tasks")
         "* TODO %? :waiting:")
        ("c" "commitment" entry (file+headline "~/org/task.org" "tasks")
         "* TODO %? :commitment:\nDEADLINE: %t")))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (setq org-habit-graph-column 45)
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)
            (define-key org-agenda-mode-map (kbd "y") 'org-agenda-todo-yesterday)
            (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-open-link)))

(setq org-deadline-warning-days 0) ; warn me of any deadlines in the next 5 days
(setq org-agenda-hide-tags-regexp ".")
(setq pbl-header-length 72)
(setq pbl-header-pad ?\ )

(defun pbl-right-pad-header (tag)
  "Make a header that has a certain length"
  ;(pbl-propertize-string
  (concat
   tag
   " "
   (make-string (- pbl-header-length (length tag)) pbl-header-pad)))

(setq pbl-org-agenda-deadline-sparkline-size 15)
(setq pbl-org-agenda-deadline-sparkline-dot ?·)
(setq pbl-org-agenda-deadline-sparkline-bar ?-)
(setq pbl-org-agenda-deadline-sparkline-overdue ?!)
(setq pbl-org-agenda-deadline-sparkline-start "|")
(setq pbl-org-agenda-deadline-sparkline-end "|")
(setq pbl-org-agenda-deadline-sparkline-date-separator "/")

(defun pbl-org-agenda-display-deadline-sparkline ()
  "Display sparkline from deadline information"
  (let ((deadline (org-entry-get nil "DEADLINE")))
    (when deadline
      (let* ((days-remaining (org-time-stamp-to-now deadline))
             (date-parts (split-string deadline "-"))
             (month (nth 1 date-parts))
             (day-parts (split-string (nth 2 date-parts)))
             (day (nth 0 day-parts))
             (num-bars (-
                        (/ days-remaining 7)
                        (if (= (% days-remaining 7) 0) 1 0)))
             (num-dots (if (> num-bars 0) 7
                         (if (> days-remaining 0) days-remaining)))
             (num-others (if (>= days-remaining 0) 1
                           (if (< days-remaining 0) 2))))
        (concat (pbl-pad-val
                 (if (<= days-remaining -10)
                     "!!"
                   (int-to-string days-remaining))
                 2)
                pbl-org-agenda-deadline-sparkline-start
                (pbl-pad-val
                 (concat
                  (if (> num-bars 0)
                      (make-string num-bars pbl-org-agenda-deadline-sparkline-bar))
                  (if (> days-remaining 1)
                      (make-string num-dots pbl-org-agenda-deadline-sparkline-dot))
                  (if (= days-remaining 1)
                      (make-string 1 pbl-org-agenda-deadline-sparkline-dot))
                  (if (= days-remaining 0)
                      (make-string 1 pbl-org-agenda-deadline-sparkline-overdue))
                  (if (< days-remaining 0)
                      (make-string 2 pbl-org-agenda-deadline-sparkline-overdue)) pbl-org-agenda-deadline-sparkline-end)
                 pbl-org-agenda-deadline-sparkline-size)
                month
                pbl-org-agenda-deadline-sparkline-date-separator
                day
                " ")))))

; Defaults that will be overriden if necessary
(setq org-agenda-block-separator "")
(setq org-agenda-prefix-format "")
(setq org-agenda-span 1)
(setq org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
(setq org-agenda-add-newline-before-block-separator nil)

(defun pbl--org-agenda-files (&rest files)
  (cl-loop for f in files collect (expand-file-name (concat f ".org") org-directory)))

(setq org-agenda-files (pbl--org-agenda-files "task"))

(defun sl-get-padded-todo-parent (size)
  "Return string of length SIZE containing either padded or truncated parent name."
  (let* ((parent (cadr (org-get-outline-path)))
         (padding (- size (length parent))))
    (if (< padding 0) (substring parent 0 size)
       (concat parent (make-string padding ?\ )))))

;; Note 20190916: This would make a good blog post
(defun sl-format-project-prefix ()
  "Format project prefix to show parent heading"
  (let* ((project-name (cadr (org-get-outline-path)))
         (project-name-size 20)
         (project-name-length (length project-name))
         (num-spaces (- project-name-size project-name-length)))
    (concat
     (if (< num-spaces 0)
         (substring project-name 0 project-name-size)
       (concat project-name (make-string num-spaces ?\ )))
     " "
     (sl-get-sparkline (sl-get-stats-cookie))
     " ")))

(defun sl-get-stats-cookie ()
  "Get stats cookie of parent heading of current todo task"
  (save-excursion
    (let* ((header (progn
                     (org-up-heading-safe)
                     (thing-at-point 'line)))
           (split-header (split-string header))
           (header-length (safe-length split-header))
           (cookie (nth (- header-length 2) split-header)))
      (substring-no-properties cookie))))

(defun sl-get-sparkline (stats)
  "Display sparkline showing progress from STATS, or nothing if STATS is not well-formed"
  (if (char-equal (string-to-char (substring stats 0 1)) ?\[)
      (let* ((stats-int (replace-regexp-in-string "\\[\\|\\]\\|%" "" stats))
             (vals (split-string stats-int "/"))
             (numerator (string-to-number (car vals)))
             (denominator (string-to-number (cadr vals)))
             (stats-float (/ (float numerator) denominator))
             (sparkline-size 15)
             (num-bars (truncate (* stats-float sparkline-size)))
             (num-spaces (- sparkline-size num-bars)))
        (concat
         (number-to-string numerator)
         "|"
         (make-string num-bars ?- )
         (make-string num-spaces ?\ )
         "|"
         (number-to-string denominator)
    ""))))

(setq org-agenda-custom-commands
      '(("c" "custom daily view"
         (;(tags-todo "frog+TODO=\"TODO\""
          ;           ((org-agenda-files (pbl--org-agenda-files "task"))
          ;            (org-agenda-overriding-header (pbl-right-pad-header "FROGS"))))
          (tags-todo "weeklygoal+TODO=\"TODO\""
                     ((org-agenda-files (pbl--org-agenda-files "goal"))
                      (org-agenda-overriding-header (pbl-right-pad-header "GOALS"))))
          (agenda "" ((org-agenda-files (pbl--org-agenda-files "task"))
                      (org-agenda-span 4)
                      (org-agenda-overriding-header (pbl-right-pad-header "AGENDA"))))
          (agenda "" ((org-agenda-files (pbl--org-agenda-files "habit"))
                      (org-agenda-span 1)
                      (org-agenda-overriding-header (pbl-right-pad-header "HABITS"))))
          (tags-todo "-{.*}+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "TODO"))))
          (tags-todo "commitment+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "COMMITMENTS"))))
          (tags-todo "waiting+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "WAITING ON"))))
          (tags-todo "active+next+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "PROJECTS"))
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")))
          (tags-todo "idea+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "IDEAS"))))
          (tags-todo "inbox+TODO=\"TODO\""
                     ((org-agenda-overriding-header (pbl-right-pad-header "INBOX"))))))))
(pbl--profile "define functions")

(pbl--profile "load then quit")
(org-agenda nil "c")
(org-agenda-quit)
(pbl--profile "load then quit")

; overwrite function - exactly the same as "org-agenda.el"
; except I add conditional on whether to add newline before block-separator
(defun org-agenda-prepare (&optional name)
  (let ((filter-alist (when org-agenda-persistent-filter
			(with-current-buffer
			    (get-buffer-create org-agenda-buffer-name)
			  `((tag . ,org-agenda-tag-filter)
			    (re . ,org-agenda-regexp-filter)
			    (effort . ,org-agenda-effort-filter)
			    (cat . ,org-agenda-category-filter))))))
    (if (org-agenda-use-sticky-p)
	(progn
	  (put 'org-agenda-tag-filter :preset-filter nil)
	  (put 'org-agenda-category-filter :preset-filter nil)
	  (put 'org-agenda-regexp-filter :preset-filter nil)
	  (put 'org-agenda-effort-filter :preset-filter nil)
	  ;; Popup existing buffer
	  (org-agenda-prepare-window (get-buffer org-agenda-buffer-name)
				     filter-alist)
	  (message "Sticky Agenda buffer, use `r' to refresh")
	  (or org-agenda-multi (org-agenda-fit-window-to-buffer))
	  (throw 'exit "Sticky Agenda buffer, use `r' to refresh"))
      (setq org-todo-keywords-for-agenda nil)
      (put 'org-agenda-tag-filter :preset-filter
	   org-agenda-tag-filter-preset)
      (put 'org-agenda-category-filter :preset-filter
	   org-agenda-category-filter-preset)
      (put 'org-agenda-regexp-filter :preset-filter
	   org-agenda-regexp-filter-preset)
      (put 'org-agenda-effort-filter :preset-filter
	   org-agenda-effort-filter-preset)
      (if org-agenda-multi
	  (progn
	    (setq buffer-read-only nil)
	    (goto-char (point-max))
	    (unless (or (bobp) org-agenda-compact-blocks
			(not org-agenda-block-separator))
	      (insert (if org-agenda-add-newline-before-block-separator "\n" "")
		      (if (stringp org-agenda-block-separator)
			  org-agenda-block-separator
			(make-string (window-width) org-agenda-block-separator))
		     "\n" ))
	    (narrow-to-region (point) (point-max)))
	(setq org-done-keywords-for-agenda nil)
	;; Setting any org variables that are in org-agenda-local-vars
	;; list need to be done after the prepare call
	(org-agenda-prepare-window
	 (get-buffer-create org-agenda-buffer-name) filter-alist)
	(setq buffer-read-only nil)
	(org-agenda-reset-markers)
	(let ((inhibit-read-only t)) (erase-buffer))
	(org-agenda-mode)
	(setq org-agenda-buffer (current-buffer))
	(setq org-agenda-contributing-files nil)
	(setq org-agenda-columns-active nil)
	(org-agenda-prepare-buffers (org-agenda-files nil 'ifmode))
	(setq org-todo-keywords-for-agenda
	      (org-uniquify org-todo-keywords-for-agenda))
	(setq org-done-keywords-for-agenda
	      (org-uniquify org-done-keywords-for-agenda))
	(setq org-agenda-last-prefix-arg current-prefix-arg)
	(setq org-agenda-this-buffer-name org-agenda-buffer-name)
	(and name (not org-agenda-name)
	     (setq-local org-agenda-name name)))
      (setq buffer-read-only nil))))

(provide 'init-org)

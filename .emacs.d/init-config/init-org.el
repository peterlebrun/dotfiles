;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; Not sure if I should be setting these but eff it, YOLO
(set-face-attribute 'org-agenda-structure nil :foreground "LightGray" :weight 'bold :underline t)
(set-face-attribute 'org-agenda-date nil :foreground "DarkGray" :weight 'ultra-light :underline nil)
(set-face-attribute 'org-agenda-date-weekend nil :foreground "DarkGray" :weight 'ultra-light)
(set-face-attribute 'org-agenda-date-today nil :foreground "DarkGray" :weight 'ultra-light :slant 'normal)
(set-face-attribute 'org-scheduled nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-scheduled-today nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-todo nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-upcoming-deadline nil :foreground "white" :weight 'ultra-light)
(set-face-attribute 'org-warning nil :foreground "white" :weight 'ultra-light)

(setq org-modules '(org-w3m
                    org-bbdb
                    org-habit
                    org-bibtex
                    org-docview
                    org-gnus
                    org-info
                    org-irc
                    org-mhe
                    org-rmail))

;;file to save todo items
(setq org-directory "~/Dropbox/org-todo")

(setq org-archive-location "~/Dropbox/org-todo/archive.org::")

(setq org-refile-targets `((,(expand-file-name "task.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "project.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "idea.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "inbox.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "goal.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "thought.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "bookmark.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "habit.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "backlog.org" org-directory) :maxlevel . 1)))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)
(setq org-catch-invisible-edits t)
(setq org-M-RET-may-split-line nil)
(setq org-return-follows-link t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-time-leading-zero t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-window-setup 'current-window)
(setq org-export-initial-scope 'subtree)
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-stuck-projects '("+active+LEVEL=2/-COMPLETE" ("TODO")))
(setq org-agenda-use-time-grid nil) ; I don't find this useful

(setq pbl-org-agenda-project-name-size 21)
(setq pbl-org-agenda-sparkline-size 15)
(setq pbl-org-agenda-sparkline-start "|")
(setq pbl-org-agenda-sparkline-end "|")
(setq pbl-org-agenda-sparkline-body ?·) ;used as a character
(setq pbl-pad-val-size 3)

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
     "  ")))

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
  (let* ((val-string (number-to-string val))
         (num-spaces (- total-size (length val-string)))
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

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "LightGreen" :weight 'ultra-light))
        ("NOT STARTED" . org-warning)
        ("IN PROGRESS" . "yellow")
        ("CANCELED" . (:foreground "LightSteelBlue" :weight bold))
        ("DONE" . (:foreground "#8C5353" :weight 'ultra-light))
        ("COMPLETE" . (:foreground "LightSteelBlue" :weight bold))))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("i" "inbox" entry (file+headline "~/Dropbox/org-todo/inbox.org" "inbox")
         "* TODO %?")
        ("h" "habit" entry (file+headline "~/Dropbox/org-todo/habit.org" "habits")
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:STYLE: habit\n:END:")
        ("t" "task" entry (file+headline "~/Dropbox/org-todo/task.org" "tasks")
         "* TODO %?\nSCHEDULED: %t")
        ("p" "project" entry (file+headline "~/Dropbox/org-todo/project.org" "projects")
         "* NOT STARTED %?\n:PROPERTIES:\n:ORDERED:  t\n:END:")
        ;; Daily captures below
        ("f" "freewrite" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U freewriting:%?\n")
        ("a" "morning-writing" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         (file "~/Dropbox/org-todo/templates/morning-writing.org"))
        ("d" "daily-review" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         (file "~/Dropbox/org-todo/templates/daily-review.org"))
        ("w" "weekly-review" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         (file "~/Dropbox/org-todo/templates/weekly-review.org"))
        ("c" "appointment" entry (file+headline "~/Dropbox/org-todo/task.org" "tasks")
         "* TODO %?\nDEADLINE: %t")
        ("m" "most-important-task" entry (file+headline "~/Dropbox/org-todo/goal.org" "goals")
         "* TODO %?:mit:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
        ("j" "daily-goals" entry (file+headline "~/Dropbox/org-todo/goal.org" "goals")
         "
* TODO %?                                                 :daily:
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))
* TODO                                                    :daily:
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))
* TODO                                                    :daily:
SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))")
        ("k" "weekly-goals" entry (file+headline "~/Dropbox/org-todo/goal.org" "goals")
         "* TODO %?:weekly:\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+1w\"))")))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (setq org-habit-graph-column 45)
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)
            (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-open-link)))

(setq org-deadline-warning-days 3) ; warn me of any deadlines in the next 5 days
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

; Defaults that will be overriden if necessary
(setq org-agenda-block-separator "")
(setq org-agenda-prefix-format "")
(setq org-agenda-span 1)
(setq org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
(setq org-agenda-add-newline-before-block-separator nil)

(defun pbl-org-agenda-files (&rest files)
  (loop for f in files collect (expand-file-name (concat f ".org") org-directory)))

; @TODO: Replace org-agenda-files with macro expansion?
; @TODO: Generate "org-agenda-custom-commands" via macro expansion that hides empty blocks
(setq org-agenda-custom-commands
      '(("c" "custom daily view"
         ((tags-todo "high+next+TODO=\"TODO\""
                     ((org-agenda-files (pbl-org-agenda-files "goal"))
                      (org-agenda-overriding-header (pbl-right-pad-header "HIGH PRIORITY 2020 GOALS"))
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")))
          (agenda "" ((org-agenda-files (pbl-org-agenda-files "task" "project" "goal" "habit"))
                      (org-agenda-span 4)
                      (org-agenda-overriding-header (pbl-right-pad-header "AGENDA"))))
          (tags-todo "active+next+TODO=\"TODO\""
                     ((org-agenda-files (pbl-org-agenda-files "project"))
                      (org-agenda-overriding-header (pbl-right-pad-header "PROJECTS"))
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")))
          (tags-todo "low+next+TODO=\"TODO\""
                     ((org-agenda-files (pbl-org-agenda-files "goal"))
                      (org-agenda-overriding-header (pbl-right-pad-header "LOW PRIORITY 2020 GOALS"))
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")))
          (tags-todo "category=\"bookmark\"+TODO=\"TODO\""
                     ((org-agenda-files (pbl-org-agenda-files "bookmark"))
                      (org-agenda-max-entries 1)
                      (org-agenda-overriding-header (pbl-right-pad-header "BOOKMARKS"))))
          (tags-todo "CATEGORY=\"task\""
                     ((org-agenda-files (pbl-org-agenda-files "task"))
                      (org-agenda-skip-function '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-overriding-header (pbl-right-pad-header "TASKS"))))
          (tags-todo "CATEGORY=\"inbox\""
                     ((org-agenda-files (pbl-org-agenda-files "inbox"))
                      (org-agenda-overriding-header (pbl-right-pad-header "INBOX"))))))
        ("g" "goals view"
         ((tags-todo "next+TODO=\"TODO\""
                     ((org-agenda-files (pbl-org-agenda-files "goal"))
                      (org-agenda-overriding-header (pbl-right-pad-header "2020 GOALS"))
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")))))))

;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; don't give a warning color to tasks with impending deadlines
;; if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))

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
		      "\n"))
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

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

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

(setq pbl-org-agenda-project-name-size 22)
(setq pbl-org-agenda-sparkline-size 10)
(setq pbl-org-agenda-sparkline-start "|")
(setq pbl-org-agenda-sparkline-body ?·) ;used as a character

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
     sparkline)))

(defun pbl-get-stats-cookie ()
  "Get stats cookie of parent heading of current todo task"
  (let* ((current-position (point))
         (header (progn
                   (org-up-heading-safe)
                   (thing-at-point 'line)))
         (split-header (split-string header))
         (header-length (safe-length split-header))
         (cookie (nth (- header-length 2) split-header)))
    (goto-char current-position)
    (substring-no-properties cookie)))

;; Note 20191205: This would make a great blog post
(defun pbl-get-sparkline (stats)
  "Display sparkline showing progress from STATS, or nothing if STATS is not well-formed"
  (if (char-equal (string-to-char (substring stats 0 1)) ?\[)
      (let* ((stats-int (replace-regexp-in-string "\\[\\|\\]\\|%" "" stats))
             (stats-float (/ (float (string-to-number stats-int)) 100))
             (sparkline-size pbl-org-agenda-sparkline-size)
             (num-bars (truncate (* stats-float sparkline-size)))
             (num-spaces (- sparkline-size num-bars)))
        (concat
         pbl-org-agenda-sparkline-start
         (make-string num-bars pbl-org-agenda-sparkline-body)
         (make-string num-spaces ?\ )
         (if (< (string-to-number stats-int) 10) " ")
         stats-int
         "%"))
    ""))

(setq org-confirm-elisp-link-not-regexp "org-capture.*")

(defun org-summary-todo (n-done n-not-done)
  "Switch (sub)project to COMPLETE when all subtasks are done, to IN PROGRESS otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (if (= n-done 0) (org-todo "NOT STARTED")
      (if (> n-not-done 0) (org-todo "IN PROGRESS")
        (progn
          (org-todo "COMPLETE")
          (org-set-tags-to (seq-remove
                            (lambda (e) (string= e "active"))
                            (org-get-tags))))))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

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

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NOT STARTED" . org-warning)
        ("IN PROGRESS" . "yellow")
        ("CANCELED" . (:foreground "LightSteelBlue" :weight bold))
        ("DONE" . (:foreground "LightSteelBlue" :weight bold))
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
        ("b" "bookmark" entry (file+headline "~/Dropbox/org-todo/bookmark.org" "bookmarks")
         "* TODO %?")
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
        ("o" "thought" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U thought\n-%?")
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
            (setq org-habit-graph-column 53)
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)
            (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-open-link)))

;; warn me of any deadlines in the next 7 days
(setq org-deadline-warning-days 7)
;; show me tasks scheduled or due in next week
(setq org-agenda-span 'day)
(setq org-agenda-hide-tags-regexp ".")

(setq org-agenda-custom-commands
      '(("c" "custom daily view"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-prefix-format "  %-7T ")
                      (org-agenda-files (list (expand-file-name "goal.org" org-directory)))
                      (org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)))
                      (org-agenda-overriding-header "Goals")))
          (agenda "" ((org-agenda-span 5)
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-files (list (expand-file-name "task.org" org-directory)
                                              (expand-file-name "project.org" org-directory)))
                      (org-agenda-overriding-header "Today's Scheduled Tasks")))
          (tags-todo "category=\"bookmark\"+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Bookmarks")
                      (org-agenda-max-entries 1)
                      (org-agenda-files (list (expand-file-name "bookmark.org" org-directory)))
                      (org-agenda-prefix-format "  ")))
          (agenda ""
                  ((org-agenda-prefix-format " %-12T ")
                   (org-agenda-files (list (expand-file-name "habit.org" org-directory)))i
                   (org-agenda-sorting-strategy '(tag-up))
                   (org-agenda-overriding-header "Habits")))
          (tags-todo "active+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Projects")
                      (org-agenda-prefix-format " %-5T  %(pbl-format-project-prefix)  ")
                      (org-agenda-sorting-strategy '(tag-up))
                      (org-agenda-files (list (expand-file-name "project.org" org-directory)))
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "paused+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Paused Projects")
                      (org-agenda-prefix-format " %-5T  %(pbl-format-project-prefix)  ")
                      (org-agenda-block-separator nil)
                      (org-agenda-files (list (expand-file-name "project.org" org-directory)))
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-block-separator nil)
                      (org-agenda-files (list (expand-file-name "project.org" org-directory)))
                      (org-agenda-prefix-format " %-5(concat \"stuck\")  ")))
          (tags-todo "CATEGORY=\"inbox\"+TODO=\"TODO\""
                     ((org-agenda-files (list (expand-file-name "inbox.org" org-directory)))
                      (org-agenda-max-entries 15)
                      (org-agenda-overriding-header "inbox")))
          (tags-todo "CATEGORY=\"task\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-files (list (expand-file-name "task.org" org-directory)))
                      (org-agenda-max-entries 15)
                      (org-agenda-overriding-header "tasks")))))))

(defun pbl--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it does not have a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; don't give a warning color to tasks with impending deadlines
;; if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;; don't show tasks that are scheduled or have deadlines in the
;; normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;; sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))

(provide 'init-org)

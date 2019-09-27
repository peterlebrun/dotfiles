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
(setq org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                             (expand-file-name "social.org" org-directory)
                             (expand-file-name "project.org" org-directory)
                             (expand-file-name "task.org" org-directory)
                             (expand-file-name "calendar.org" org-directory)
                             (expand-file-name "bookmark.org" org-directory)
                             (expand-file-name "habit.org" org-directory)))

(setq org-archive-location "~/Dropbox/org-todo/archive.org::")

(setq org-refile-targets `((,(expand-file-name "task.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "social.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "project.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "idea.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "goal.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "calendar.org" org-directory) :maxlevel . 1)
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
(setq org-agenda-diary-file (expand-file-name "diary.org" org-directory))
(setq org-stuck-projects '("+active+LEVEL=2/-COMPLETE" ("TODO")))
(setq org-agenda-use-time-grid nil) ; I don't find this useful
;; Note 20190916: This would make good blog post
(defun pbl-format-project-prefix ()
  (let ((outline-list (org-get-outline-path)))
    (concat
     "  "
     (cadr outline-list)
     (if (> (length outline-list) 2) " → ..." "")
     " → ")))
(setq org-confirm-elisp-link-not-regexp "org-capture.*")

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
         "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a>>")
        ("b" "bookmark" entry (file+headline "~/Dropbox/org-todo/bookmark.org" "bookmarks")
         "* TODO %?")
        ("p" "project" entry (file+headline "~/Dropbox/org-todo/project.org" "projects")
         "* NOT STARTED %?\n:PROPERTIES:\n:ORDERED:  t\n:END:")
        ;; Daily captures below
        ("g" "gratitude-list" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U gratitude list\n-%?")
        ("l" "random-list" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U random list:%?\n-")
        ("f" "freewrite" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U freewriting:%?\n")
        ("d" "daily-review" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U daily-review
- productivity ::
- mood ::
- anxiety ::
- energy ::
- Today I wanted to accomplish
  + %?
- [ ] Determine tomorrow's most important task
- [ ] Assign tasks for tomorrow
- [ ] Review tomorrow's calendar
- [ ] How did I feel about my productivity?
- [ ] How did I feel about my mood?
- [ ] How did I feel about my anxiety?
- [ ] How did I feel about my energy?
- [ ] What do I want to accomplish tomorrow?
- [ ] Were we resentful, selfish, dishonest or afraid?
- [ ] Do we owe an apology?
- [ ] Have we kept something to ourselves which should be discussed with another person at once?
- [ ] Were we kind and loving toward all?
- [ ] What could we have done better?
- [ ] Were we thinking of ourselves most of the time?  Or were we thinking of what we could do for others, of what we could pack into the stream of life?")
        ("w" "weekly-review" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U weekly-review
- My goals for this week were
  + %?
- I have accomplished
  +
- I have not accomplished (because...)
  +
- I would like to acknowledge myself for
  +
- During this week's coaching session I would like to focus on
  +
- Appointments this coming week
  +
- This week I want to accomplish the following
  +")
        ("o" "thought" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U thought\n-%?")
        ("c" "appointment" entry (file+headline "~/Dropbox/org-todo/calendar.org" "calendar")
         "* TODO %?\nDEADLINE: <%<%Y-%m-%d %a>>")))

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
(setq org-agenda-hide-tags-regexp "active\\|project\\|book\\|work\\|home\\|class")

(setq org-agenda-custom-commands
      '(("c" "custom daily view"
         ((agenda "" ((org-agenda-span 5)
                      (org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)
                            (pbl--org-skip-subtree-if-habit)))
                      (org-agenda-overriding-header "Today's Scheduled Tasks")))
          (tags-todo "category=\"bookmark\"+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Bookmarks")
                      (org-agenda-max-entries 1)
                      (org-agenda-prefix-format "  ")))
          (tags-todo "active+work+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Projects: Work")
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "active+home+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Projects: Home")
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "active+book+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Books")
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "active+class+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Classes")
                      (org-agenda-prefix-format "%(pbl-format-project-prefix)")
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-prefix-format "  ")))))
        ("i" "inbox view"
         ((tags-todo "CATEGORY=\"inbox\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-overriding-header "inbox")))
          (tags-todo "CATEGORY=\"task\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-overriding-header "tasks")))))
        ("h" "habit view"
         ((agenda ""
                     ((org-agenda-skip-function
                       (or
                        '(org-agenda-skip-entry-if 'notregexp ":am:")
                        '(pbl--org-skip-subtree-if-not-habit)))
                      (org-agenda-overriding-header "Morning Routine")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":food:")))
                      (org-agenda-overriding-header "Habits: Food")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":productivity:")))
                      (org-agenda-overriding-header "Habits: Productivity")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":growth:")))
                      (org-agenda-overriding-header "Habits: Growth")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":learning:")))
                      (org-agenda-overriding-header "Habits: Learning")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":healing:")))
                      (org-agenda-overriding-header "Habits: Healing")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda "" ((org-agenda-skip-function
                       '(or (pbl--org-skip-subtree-if-not-habit)
                            (org-agenda-skip-entry-if 'notregexp ":social:")))
                      (org-agenda-overriding-header "Habits: Social")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda ""
                     ((org-agenda-skip-function
                       (or
                        '(org-agenda-skip-entry-if 'notregexp ":pm:")
                        '(pbl--org-skip-subtree-if-not-habit)))
                      (org-agenda-overriding-header "Evening Routine")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))
          (agenda ""
                     ((org-agenda-skip-function
                       (or
                        '(org-agenda-skip-entry-if 'notregexp ":grooming:")
                        '(pbl--org-skip-subtree-if-not-habit)))
                      (org-agenda-overriding-header "Grooming")
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-prefix-format "  ")))))))

(defun air--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  "Skip an agenda entry if it has a STYLE property equal to \"priority\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "PRIORITY") "A")
        subtree-end
      nil)))

(defun pbl--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it does not have a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(defun pbl--org-skip-subtree-if-not-habit ()
  "Skip an agenda entry if it does not have a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (not (string= (org-entry-get nil "STYLE") "habit"))
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

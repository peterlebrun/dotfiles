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
     (if (> (length outline-list) 2) " ..." "")
     " ")))
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
         "* TODO %?\nSCHEDULED: %t")
        ("b" "bookmark" entry (file+headline "~/Dropbox/org-todo/bookmark.org" "bookmarks")
         "* TODO %?")
        ("p" "project" entry (file+headline "~/Dropbox/org-todo/project.org" "projects")
         "* NOT STARTED %?\n:PROPERTIES:\n:ORDERED:  t\n:END:")
        ;; Daily captures below
        ("f" "freewrite" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U freewriting:%?\n")
        ("a" "morning-writing" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U morning writing
- what are my five life goals??
  + %?
- what are today's three goals?
  +
- gratitude list
  +
- one game idea
  +
- ten opportunities
  1.
- ten problems to solve
  1.
- ten ways I can provide value
  1.
- morning freewriting")
        ("d" "daily-review" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U daily-review
- what are my five life goals??
  + %?
- productivity ::
  +
- mood ::
  +
- anxiety ::
  +
- energy ::
  +
- Today I wanted to accomplish
  +
- [ ] [[elisp:(org-capture nil \"m\")][Determine Tomorrow's Most Important Task]]
- [ ] [[elisp:(org-capture nil \"j\")][Set Daily Goals]]
- [ ] Review tomorrow's calendar
- Was I resentful, selfish, self-seeking, dishonest or afraid?
  +
- Do I owe an apology?
  +
- Have I kept something to myself which should be discussed with another
  person at once?
  +
- Was I kind and loving toward all?
  +
- What could I have done better?
  +
- Was I thinking of ourselves most of the time?  Or was I thinking of
  what I could do for others, of what I could pack into the stream of
  life?
  +
- cathartic freewriting")
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
- [[elisp:(org-capture nil \"k\")][Set Weekly Goals]]")
        ("o" "thought" entry (file+olp+datetree "~/Dropbox/org/pensieve.org" "pensieve")
         "* %U thought\n-%?")
        ("c" "appointment" entry (file+headline "~/Dropbox/org-todo/calendar.org" "calendar")
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
(setq org-agenda-hide-tags-regexp "active\\|project\\|book\\|work\\|home\\|class\\|paused")

(setq org-agenda-custom-commands
      '(("c" "custom daily view"
         ((agenda "" ((org-agenda-span 1)
                      (org-agenda-prefix-format "  %-7T ")
                      (org-agenda-files (list (expand-file-name "goal.org" org-directory)))
                      (org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)))
                      (org-agenda-hide-tags-regexp ".")
                      (org-agenda-overriding-header "Goals")))
          (agenda "" ((org-agenda-span 5)
                      (org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)
                            (pbl--org-skip-subtree-if-habit)))
                      (org-agenda-overriding-header "Today's Scheduled Tasks")))
          (tags-todo "category=\"bookmark\"+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Bookmarks")
                      (org-agenda-max-entries 1)
                      (org-agenda-prefix-format "  ")))
          (tags-todo "active+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Projects: Next Steps")
                      (org-agenda-prefix-format "  %-6T %-30(pbl-format-project-prefix)")
                      (org-agenda-sorting-strategy '(tag-up))
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (tags-todo "paused+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Paused Projects")
                      (org-agenda-prefix-format "  %-6T %-30(pbl-format-project-prefix)")
                      (org-agenda-block-separator nil)
                      (org-agenda-dim-blocked-tasks 'invisible)))
          (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-block-separator nil)
                      (org-agenda-prefix-format "  %-6(concat \"stuck\")")))
          (tags-todo "CATEGORY=\"inbox\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-overriding-header "inbox")))
          (tags-todo "CATEGORY=\"task\""
                     ((org-agenda-skip-function
                       '(org-agenda-skip-if nil '(scheduled deadline)))
                      (org-agenda-overriding-header "tasks")))))
        ("h" "habit view"
         ((agenda ""
                  ((org-agenda-compact-blocks t)
                   (org-agenda-prefix-format " %-12T ")
                   (org-agenda-files (list (expand-file-name "habit.org" org-directory)))
                   (org-agenda-sorting-strategy '(tag-up))
                   (org-agenda-hide-tags-regexp ".")))))))

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

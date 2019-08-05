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
                             (expand-file-name "anniversary.org" org-directory)
                             (expand-file-name "book.org" org-directory)
                             (expand-file-name "class.org" org-directory)))

(setq org-archive-location "~/Dropbox/org-todo/archive.org::")

(setq org-refile-targets `((,(expand-file-name "task.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "social.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "project.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "idea.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "goal.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "calendar.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "anniversary.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "thought.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "book.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "bookmark.org" org-directory) :maxlevel . 1)
                           (,(expand-file-name "class.org" org-directory) :maxlevel . 1)
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
; Notice that we are setting ORG-AGENDA-TIME-GRID to its default value
; Except we've reversed the times to display
; I think this is just a weird bug, not sure
(setq org-agenda-time-grid '((daily today require-timed)
                             (2000 1800 1600 1400 1200 1000 0800)
                             "......" "----------------"))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-habit-show-habits-only-for-today t)

; Open question 20190801: if I have the same state in both subsequences,
; will that cause problems? Motivation: I was getting issues where
; it seemed like I was jumping between sequences when both had a state
; called "IN PROGRESS"
; sequence 1: task states
; sequence 2: project states
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")
        (sequence "NOT STARTED" "IN PROGRESS(!)" "|" "COMPLETE")))

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
         "* TODO %?")))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (setq org-habit-graph-column 50)
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)))

;; warn me of any deadlines in the next 7 days
(setq org-deadline-warning-days 7)
;; show me tasks scheduled or due in next week
(setq org-agenda-span 'day)
(setq org-agenda-hide-tags-regexp "active\\|project")

(setq org-agenda-custom-commands
      '(("c" "daily view"
         ((tags-todo "PRIORITY=\"A\""
                     ((org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)
                            (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "High priority tasks")
                      (org-agenda-dim-blocked-tasks 't)))
          (tags-todo "category=\"bookmark\"+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Bookmarks To Read")
                      (org-agenda-files (list (expand-file-name "bookmark.org" org-directory)))
                      (org-agenda-max-entries 1)))
          (agenda "" ((org-agenda-ndays-to-span 1)
                      (org-agenda-skip-function
                       '(or (org-agenda-skip-entry-if 'todo 'done)))))
          (tags-todo "active+TODO=\"TODO\",project+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Active Projects: Next Steps")
                      (org-agenda-prefix-format "  %b")
                      (org-agenda-dim-blocked-tasks 'invisible)))))
          ;(tags-todo "CATEGORY=\"task\""
          ;           ((org-agenda-skip-function
          ;             '(or (air--org-skip-subtree-if-habit)
          ;                  (air--org-skip-subtree-if-priority ?A)
          ;                  (org-agenda-skip-if nil '(scheduled deadline))))
          ;            (org-agenda-overriding-header "Unscheduled tasks")))))
        ("d" "dream view"
         ((tags-todo "CATEGORY=\"goal\"" ((org-agenda-overriding-header "goals")))
          (tags-todo "CATEGORY=\"inbox\"" ((org-agenda-overriding-header "inbox")))
          (tags-todo "CATEGORY=\"project\"" ((org-agenda-overriding-header "projects")))))))

(defun air--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "PRIORITY") "A")
        subtree-end
      nil)))

(defun air--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
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

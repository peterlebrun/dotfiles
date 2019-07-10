;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;file to save todo items
(setq org-agenda-files (quote ("~/Dropbox/org-todo/todo.org")))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/Dropbox/org-todo/todo.org" "Tasks")
         "* TODO %?")))

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-item)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-item)))

;; org-mode agenda options
;; open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;; warn me of any deadlines in the next 7 days
(setq org-deadline-warning-days 7)
;; show me tasks scheduled or due in next week
(setq org-agenda-span (quote week))

(setq org-agenda-custom-commands
      '(("c" "Simple composite view"
         ((tags-todo "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays-to-span 1)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air--org-skip-subtree-if-habit)
                          (air--org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))
                          (org-agenda-skip-entry-if 'regexp ":inbox:")
                          (org-agenda-skip-entry-if 'regexp ":maybe:")
                          (org-agenda-skip-entry-if 'regexp ":tocall:")
                          (org-agenda-skip-entry-if 'regexp ":guitar:")
                          (org-agenda-skip-entry-if 'regexp ":toread:")))))
          (tags ":inbox:" ((org-agenda-overriding-header "Inbox"))) ))))
          ;(alltodo ""
          ;         ((org-agenda-skip-function
          ;           (org-agenda-skip-if 'notregexp ":inbox:"))))))))

(defun air--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
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

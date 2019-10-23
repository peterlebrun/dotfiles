;;; init-evil.el --- Evil mode configuration
;;; Commentary:
;;; Code:

;; Provide named methods instead of lambda expressions
;; so that mode-map (see leader ?) displays names
;; instead of ??
(defun pbl--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "RET" 'pbl--yarn-test
   "," 'pbl--open-shell
   "/" 'rgrep
   "|" 'split-window-right
   "-" 'split-window-below
   "0" 'delete-window
   ;"a" 'pbl--insert-file-contents-from-helm-search
   "aa" 'pbl--org-agenda
   "ah" 'pbl--org-agenda-habit
   "as" 'pbl--org-agenda-stuck
   "b" 'helm-mini
   "B" 'pbl--magit-blame-toggle
   "cb" 'pbl--org-capture-bookmark
   "ci" 'pbl--org-capture-inbox
   "ch" 'pbl--org-capture-habit
   "ct" 'pbl--org-capture-task
   "cp" 'pbl--org-capture-project
   "cf" 'pbl--org-capture-freewrite
   "co" 'pbl--org-capture-thought
   "cd" 'pbl--org-capture-daily-review
   "cw" 'pbl--org-capture-weekly-review
   "cc" 'pbl--org-capture-calendar
   "d" 'pbl--open-writing-file-for-today
   "eo" 'pbl--open-org-config  ; emacs config for org
   "ee" 'pbl--open-evil-config ; emacs config for evil
   "ei" 'pbl--open-init-config ; emacs config for init
   "f" 'helm-projectile
   "F" 'helm-projectile-switch-project
   "g" 'magit-status
   "j" 'pbl--insert-file-contents-from-helm-search
   "k" 'kill-buffer
   "l" 'pbl--load-current-file
	 "nl" 'pbl--narrow-to-line
	 "nn" 'pbl--narrow-to-next-line
   "o" 'other-window
   "r" 'toggle-frame-maximized
   "w" 'widen
   "x" 'helm-M-x
   "z" 'pbl--open-zshrc)

  (defun pbl--open-shell ()
    "Open shell."
   (interactive) (ansi-term (getenv "SHELL")))

  (defun pbl--open-org-config ()
    "Open org config."
   (interactive) (find-file "~/.emacs.d/init-config/init-org.el"))

  (defun pbl--open-evil-config ()
    "Open evil config."
   (interactive) (find-file "~/.emacs.d/init-config/init-evil.el"))

  (defun pbl--open-init-config ()
    "Open init config."
   (interactive) (find-file "~/.emacs.d/init.el"))

  (defun pbl--open-orgfile ()
    "Open org file."
   (interactive) (find-file "~/Dropbox/org/org.org"))

  (defun pbl--open-zshrc ()
    "Open .zshrc."
    (interactive) (find-file "~/.zshrc"))

  (defun pbl--load-current-file ()
    "Load current file."
    (interactive)
    (if (and buffer-file-name (member "el" (split-string (buffer-file-name) "\\." t)))
        (load-file (buffer-file-name))
      (message "Not editing emacs lisp file.")))

  (defun pbl--magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))

  ; Switching this to org file
  (defun pbl--open-writing-file-for-today ()
    ""
    (interactive)
    (find-file (concat "~/writings/" (format-time-string "%Y%m%d-%H%M%S") ".org"))
    (insert (concat "* " (format-time-string "%Y%m%d:%H%M\n")
                    "** Freewheeling Thoughts")))

  (defun pbl--insert-file-contents-from-helm-search ()
    "Use helm to find a file whose contents will be entered into current buffer"
    (interactive)
    (insert-file-contents (helm-read-file-name "")))

  (defun pbl--org-agenda ()
    "Open custom agenda composite view."
    (interactive)
    (org-agenda nil "c"))

  (defun pbl--org-agenda-habit ()
    "Open custom agenda habit view"
    (interactive)
    (org-agenda nil "h"))

  (defun pbl--org-agenda-stuck ()
    "Open custom agenda stuck projects view"
    (interactive)
    (org-agenda nil "#"))

  (defun pbl--org-capture-bookmark ()
    "Capture new bookmark to read"
    (interactive)
    (org-capture nil "b"))

  (defun pbl--org-capture-inbox ()
    "Capture new entry in inbox"
    (interactive)
    (org-capture nil "i"))

  (defun pbl--org-capture-habit ()
    "Capture new habit"
    (interactive)
    (org-capture nil "h"))

  (defun pbl--org-capture-task ()
    "Capture new task, scheduled for today"
    (interactive)
    (org-capture nil "t"))

  (defun pbl--org-capture-project ()
    "Capture new project"
    (interactive)
    (org-capture nil "p"))

  (defun pbl--org-capture-freewrite ()
    "Capture new project"
    (interactive)
    (org-capture nil "f"))

  (defun pbl--org-capture-thought ()
    "Capture new project"
    (interactive)
    (org-capture nil "o"))

  (defun pbl--org-capture-daily-review ()
    "Capture new project"
    (interactive)
    (org-capture nil "d"))

  (defun pbl--org-capture-weekly-review ()
    "Capture new project"
    (interactive)
    (org-capture nil "w"))

  (defun pbl--org-capture-gratitude-list ()
    "Capture new project"
    (interactive)
    (org-capture nil "g"))

  (defun pbl--org-capture-random-list ()
    "Capture new project"
    (interactive)
    (org-capture nil "l"))

  (defun pbl--org-capture-calendar ()
    "Capture new project"
    (interactive)
    (org-capture nil "c"))

  (defun pbl--yarn-test ()
    "Run yarn test for current yarn package"
    (interactive)
    (let ((test-output-buffer "*yarn-test*"))
      (shell-command "yarn test" test-output-buffer)
      (pop-to-buffer test-output-buffer)))

  ; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
  ; NOTE: This is no longer (and never was) used.  Leaving for reference.
  (defun pbl--narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

  (defun pbl--narrow-to-line ()
    "Narrow buffer to current line"
    (interactive)
    (narrow-to-region (line-beginning-position) (line-end-position)))

  (defun pbl--narrow-to-next-line ()
    "Keep buffer narrowed but move to next line.  At last line, widen and move to buffer start."
    (interactive)
    (if (buffer-narrowed-p)
        (progn
          (widen)
          (if (< (+ 1 (line-end-position)) (point-max))
              (progn
                (forward-line)
                (narrow-to-region (line-beginning-position) (line-end-position)))
            (progn
              (goto-char (point-min))
              (message "Reached end of buffer."))))
      (message "Buffer not currently narrowed.")))
  )

(use-package evil
             :ensure t
             :config
             (evil-mode 1)

             (use-package evil-leader
                         :ensure t
                         :config
                         (global-evil-leader-mode)
                         (pbl--config-evil-leader))

             (use-package evil-indent-textobject :ensure t))

(provide 'init-evil)
;;; init-evil.el ends here

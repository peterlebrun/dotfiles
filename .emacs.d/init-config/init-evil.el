;;; init-evil.el --- Evil mode configuration
;;; Commentary:
;;; Code:
(defun pbl--config-evil ()
  "Configure evil mode."
  (dolist (mode '(ewl-task-mode ewl-notes-mode spotify-playlist-search-mode spotify-track-search-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-add-hjkl-bindings ewl-task-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (evil-add-hjkl-bindings ewl-notes-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (evil-add-hjkl-bindings spotify-playlist-search-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (evil-add-hjkl-bindings spotify-track-search-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous))

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
   "ai" 'pbl--org-agenda-inbox
   "b" 'helm-mini
   "B" 'pbl--magit-blame-toggle
   "c" 'pbl--open-init-config
   "d" 'pbl--open-writing-file-for-today
   "e" 'pbl--open-org-config ;re-using keybinding for evil config
   "f" 'helm-projectile
   "F" 'helm-projectile-switch-project
   "g" 'magit-status
   "h" 'pbl--open-orgfile
   "i" 'pbl--org-add-to-inbox
   "j" 'pbl--insert-file-contents-from-helm-search
   "k" 'kill-buffer
   "l" 'pbl--load-current-file
   "m" 'next-buffer
	 "n" 'previous-buffer
   "o" 'other-window
   "r" 'toggle-frame-maximized
   "t" 'pbl--ewl-add-task-to-inbox
   "w" 'pbl--narrow-or-widen-dwim
   "x" 'helm-M-x
   "z" 'pbl--open-zshrc
   )
  (defun pbl--open-shell ()
    "Open shell."
   (interactive) (ansi-term (getenv "SHELL")))

  (defun pbl--open-org-config ()
    "Open org config."
   (interactive) (find-file "~/.emacs.d/init-config/init-org.el"))

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
    (insert (concat "* " (format-time-string "%Y%m%d:%H%M")))
    (insert "
** Gratitude List
   -
** What do I want to accomplish today?
   -
** Freewheeling Thoughts"))

  (defun pbl--insert-file-contents-from-helm-search ()
    "Use helm to find a file whose contents will be entered into current buffer"
    (interactive)
    (insert-file-contents (helm-read-file-name "")))

  (defun pbl--org-agenda ()
    "Open custom agenda composite view."
    (interactive)
    (org-agenda nil "c"))

  (defun pbl--org-agenda-habit ()
    "Open custom agenda composite view."
    (interactive)
    (org-agenda nil "h"))

  (defun pbl--org-agenda-inbox ()
    "Open custom agenda composite view."
    (interactive)
    (org-agenda nil "i"))

  (defun pbl--org-add-to-inbox ()
    "Add task to inbox"
    (interactive)
    (org-capture nil "i"))

  (defun pbl--yarn-test ()
    "Run yarn test for current yarn package"
    (interactive)
    (let ((test-output-buffer "*yarn-test*"))
      (shell-command "yarn test" test-output-buffer)
      (pop-to-buffer test-output-buffer)))

  ; Taken from http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
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
  )

(use-package evil
             :ensure t
             :config
             (add-hook 'evil-mode-hook 'pbl--config-evil)
             (evil-mode 1)

             (use-package evil-leader
                         :ensure t
                         :config
                         (global-evil-leader-mode)
                         (pbl--config-evil-leader))

             (use-package evil-indent-textobject :ensure t))

(provide 'init-evil)
;;; init-evil.el ends here

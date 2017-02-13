;;; init-evil.el --- Evil mode configuration
;;; Commentary:
;;; Code:
(defun pbl--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "," (lambda () (interactive) (ansi-term (getenv "SHELL")))
   ":" 'eval-expression
   "/" 'ag
   "-" 'helm-find-files
   "a" 'pbl--org-open-custom-agenda
   "b" 'helm-mini
   "B" 'pbl--magit-blame-toggle
   "c" 'pbl--org-task-capture
   "d" 'kill-buffer
   "e" 'split-window-right
   "f" 'helm-projectile
   "F" 'helm-projectile-switch-project
   "g" 'magit-status
   "h" 'pbl--org-habit-capture
   "i" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "l" 'load-file
   "m" 'next-buffer
	 "n" 'previous-buffer
   "r" 'toggle-frame-maximized
   "w" 'split-window-below
   "x" 'helm-M-x
   "u" (lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
   )

  (defun pbl--magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))

  (defun pbl--org-task-capture ()
    "Capture a task with a default org template."
    (interactive)
    (org-capture nil "a"))

  (defun pbl--org-habit-capture ()
    "Capture a habit with a default org template."
    (interactive)
    (org-capture nil "h"))

  (defun pbl--org-open-custom-agenda ()
    "Open my custom made org mode agenda view."
    (interactive)
    (org-agenda nil "c"))
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

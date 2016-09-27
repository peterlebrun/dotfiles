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
   "a" 'pbl--org-open-custom-agenda
   "B" 'pbl--magit-blame-toggle
   "c" 'pbl--org-task-capture
   "f" 'helm-projectile
   "F" 'helm-mini
   "g" 'magit-status
   "i" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "l" 'load-file
   "m" 'mode-line-other-buffer
   "o" (lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
   "r" 'toggle-frame-maximized)

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

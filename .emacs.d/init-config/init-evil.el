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
   "a" 'org-agenda-list
   "B" 'magit-blame-toggle
   "c" 'org-capture
   "f" 'helm-projectile
   "F" 'helm-mini
   "g" 'magit-status
   "i" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "l" 'load-file
   "n" 'mode-line-other-buffer
   "o" (lambda() (interactive) (find-file "~/Dropbox/org/todo.org"))
   "r" 'toggle-frame-maximized)

  (defun magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))
  )

(use-package evil
             :ensure t
;;             :commands (evil-mode evil-define-key)
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

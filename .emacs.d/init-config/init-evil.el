;;; init-evil.el --- Evil mode configuration
;;; Commentary:
;;; Code:
(defun pbl--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "," (lambda () (interactive) (ansi-term (getenv "SHELL")))
   ":" 'eval-expression
   "/" 'rgrep
   "|" 'split-window-right
   "-" 'split-window-below
   "0" 'delete-window
   "a" 'pbl--insert-file-contents-from-helm-search
   "b" 'helm-mini
   "B" 'pbl--magit-blame-toggle
   "D" 'pbl--open-writing-file-for-today
   "f" 'helm-projectile
   "F" 'helm-projectile-switch-project
   "g" 'magit-status
   "i" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "k" 'kill-buffer
   "l" 'load-file
   "m" 'next-buffer
	 "n" 'previous-buffer
   "o" 'other-window
   "r" 'toggle-frame-maximized
   "t" 'pbl--wunderline-add-todo
   "w" 'helm-find-files
   "x" 'helm-M-x
   "z" (lambda() (interactive) (find-file "~/.zshrc"))
   )

  (defun pbl--magit-blame-toggle ()
    "Toggle magit-blame-mode on and off interactively."
    (interactive)
    (if (and (boundp 'magit-blame-mode) magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame)))

  (defun pbl--open-writing-file-for-today ()
    ""
    (interactive)
    (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (filename (concat "~/writings/" timestamp ".txt")))
      (find-file filename)))

  (defun pbl--insert-file-contents-from-helm-search ()
    "Use helm to find a file whose contents will be entered into current buffer"
    (interactive)
    (insert-file-contents (helm-read-file-name "")))

  (defun pbl--wunderline-add-todo ()
    "Add todo to wunderlist using wunderline app"
    (interactive)
    (shell-command (concat "wunderline add \"" (read-from-minibuffer "Enter todo: ") "\"")))
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

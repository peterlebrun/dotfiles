;;; init-evil.el --- Evil mode configuration
;;; Commentary:
;;; Code:
(load-file "~/dev/code/peterlebrun/emacs-wunderlist/emacs-wunderlist.el")
(defun pbl--config-evil ()
  "Configure evil mode."
  (dolist (mode '(ewl-task-mode ewl-notes-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (evil-add-hjkl-bindings ewl-task-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous)

  (evil-add-hjkl-bindings ewl-notes-mode-map 'emacs
    (kbd "/") 'evil-search-forward
    (kbd "n") 'evil-search-next
    (kbd "N") 'evil-search-previous))

(defun pbl--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (evil-leader/set-key
   "RET" 'pbl--yarn-test
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
   "e" '(lambda() (interactive) (find-file "~/.emacs.d/init-config/init-evil.el"))
   "f" 'helm-projectile
   "F" 'helm-projectile-switch-project
   "g" 'pbl--display-gtd
   "i" (lambda() (interactive) (find-file "~/.emacs.d/init.el"))
   "k" 'kill-buffer
   "l" (lambda() (interactive) (load-file (buffer-file-name)))
   "m" 'next-buffer
	 "n" 'previous-buffer
   "o" 'other-window
   "r" 'toggle-frame-maximized
   "t" 'pbl--wunderline-add-todo
   "w" 'pbl--yarn-webpack
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
    (if (fboundp 'ewl-add-task-to-inbox) (ewl-add-task-to-inbox)
      (shell-command (concat "wunderline add \"" (read-from-minibuffer "Enter todo: ") "\""))))

  (defun pbl--display-gtd ()
    "If function is defined, use it"
    (interactive)
    (if (fboundp 'ewl-display-inbox) (ewl-display-inbox)))

  (defun pbl--yarn-test ()
    "Run yarn test for current yarn package"
    (interactive)
    (let ((test-output-buffer "*yarn-test*"))
      (shell-command "yarn test" test-output-buffer)
      (pop-to-buffer test-output-buffer)))

  (defun pbl--yarn-webpack ()
    "Run current file in node"
    (interactive)
    (let ((webpack-output-buffer "*yarn-webpack*"))
      (shell-command "yarn webpack" webpack-output-buffer)
      (pop-to-buffer webpack-output-buffer))))

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

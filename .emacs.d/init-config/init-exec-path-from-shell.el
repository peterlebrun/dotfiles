;; exec-path-from-shell enables us to use env variables between terminal/emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq exec-path (cons "/usr/local/bin/w3m" exec-path))

(provide 'init-exec-path-from-shell)

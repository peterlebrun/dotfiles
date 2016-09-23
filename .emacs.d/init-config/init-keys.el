(global-set-key (kbd "C-, c") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-, l") (lambda() (interactive) (load-file "~/.emacs.d/init.el")))

(provide 'init-keys)

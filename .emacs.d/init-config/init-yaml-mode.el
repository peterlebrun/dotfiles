(use-package yaml-mode :ensure t)
(use-package css-mode :ensure t)

(add-hook 'css-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)))

(provide 'init-yaml-mode)

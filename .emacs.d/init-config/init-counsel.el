(use-package counsel
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :ensure t)

(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))


(provide 'init-counsel)

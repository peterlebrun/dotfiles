;;; init-powerline.el - Powerline mode configuration
;;; Commentary:
;;; Code:
(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator (if (display-graphic-p) 'arrow nil))
  (powerline-default-theme))

(use-package powerline-evil :ensure t)

(provide 'init-powerline)

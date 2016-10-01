(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package ag :ensure t)
(use-package helm :ensure t)
(use-package helm-projectile :ensure t)
(use-package helm-ag :ensure t)
(use-package projectile :ensure t)
(use-package php-mode :ensure t)
(use-package php-extras :ensure t)
(use-package let-alist :ensure t)
(use-package flycheck
  :ensure t
  :config
  (evil-leader/set-key
    "e" 'flycheck-list-errors)
  (global-flycheck-mode 1))

(set-face-attribute 'default nil :font "Courier New 14") ;; set font to courier new, size 14

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.  Listed alphabetically
(require 'init-evil)
(require 'init-exec-path-from-shell)
(require 'init-magit)
(require 'init-org)
(require 'init-powerline)
(require 'init-zenburn-theme)
(require 'linum-off)

(toggle-frame-maximized)

;; Don't edit these
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company zenburn-theme powerline-evil powerline org-bullets magit exec-path-from-shell evil-indent-textobject evil-leader evil php-extras php-mode helm-ag helm-projectile helm ag use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

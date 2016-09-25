(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

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

(set-face-attribute 'default nil :font "Courier New 14") ;; set font to courier new, size 14
(toggle-frame-maximized) ;; go full screen

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.  Listed alphabetically
(require 'init-evil)
(require 'init-exec-path-from-shell)
(require 'init-magit)
(require 'init-org)
(require 'init-powerline)
(require 'init-zenburn-theme)
(require 'linum-off)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme powerline-evil powerline org-bullets magit exec-path-from-shell evil-indent-textobject evil-leader evil php-extras php-mode helm-ag helm-projectile helm ag use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

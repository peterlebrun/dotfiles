;;; Commentary:
;;; package -- summary
;;; Code:
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
(use-package helm :ensure t :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package helm-projectile :ensure t)
(use-package helm-ag :ensure t)
(use-package projectile :ensure t
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))
(use-package php-extras :ensure t)
(use-package let-alist :ensure t)
(use-package yaml-mode :ensure t)
(use-package puppet-mode :ensure t)
(use-package web-mode :ensure t)

(set-face-attribute 'default nil :font "Hack 20") ;; set font to hack, size 20

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.  Listed alphabetically
(require 'init-company)
(require 'init-evil)
(require 'init-exec-path-from-shell)
(require 'init-magit)
(require 'init-org)
(require 'init-powerline)
(require 'init-zenburn-theme)
(require 'linum-off)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(toggle-scroll-bar nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook
  (lambda ()
    (linum-mode 1)
    (setq c-basic-offset 2)))

(add-hook 'js-mode-hook
  (lambda ()
    (linum-mode 1)
    (setq js-indent-level 2)))

(add-hook 'web-mode-hook
  (lambda ()
    (linum-mode 1)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (linum-mode 1)
    (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(add-hook 'yaml-mode-hook
  (lambda ()
    (linum-mode 1)))

(toggle-frame-maximized)

;; Don't edit these
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (puppet-mode yaml-mode company zenburn-theme powerline-evil powerline org-bullets magit exec-path-from-shell evil-indent-textobject evil-leader evil php-extras php-mode helm-ag helm-projectile helm ag use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Commentary:
;;; package -- summary
;;; Code:
(require 'package)
(setq inhibit-startup-message t)

(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq-default fill-column 80)
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Visual presentation of window
(tool-bar-mode -1)
(global-visual-line-mode 1)
(toggle-frame-maximized)

(setq pbl-package-root "~/eng/github.com/peterlebrun/")
(setq create-lockfiles nil)
(setq vc-follow-symlinks t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program "/usr/local/bin/gpg2")
 '(package-selected-packages
   (quote
    (visual-fill-column writeroom-mode prettier-js w3m w3 telephone-line spotify clojure-mode unicode-fonts flow-minor-mode flow-mode org-brain org-mode flycheck-yamllint flycheck dockerfile-mode puppet-mode yaml-mode company zenburn-theme powerline-evil powerline org-bullets magit exec-path-from-shell evil-indent-textobject evil-leader evil php-mode helm-projectile helm use-package))))

(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("gnu" . "https://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(use-package rainbow-delimiters :ensure t)
(use-package visual-fill-column :ensure t)
(use-package writeroom-mode :ensure t)
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (subword-mode)
              (display-line-numbers-mode 1)
              (go-set-project)
              (company-mode t))))

(use-package prettier-js :ensure t)
(use-package rg :ensure t)
(use-package helm :ensure t :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package helm-projectile :ensure t)
(use-package projectile :ensure t
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))
(use-package let-alist :ensure t)
(use-package yaml-mode :ensure t)
(use-package puppet-mode :ensure t)
(use-package web-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package rjsx-mode :ensure t)

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/org-brain")
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  ;(push '("b" "Brain" plain (function org-brain-goto-end)
          ;"* %i%?" :empty-lines 1)
        ;org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(set-face-attribute 'default nil :font "FuraCode Nerd Font 18")

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.

(require 'init-company)
(require 'init-evil)
(require 'init-exec-path-from-shell)
(require 'init-magit)
(require 'init-zenburn-theme)
(require 'init-telephone-line)
(require 'init-org)
(require 'init-wiki2org)

(setq initial-buffer-choice (org-agenda nil "c"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yamllint\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . python-mode))
(add-to-list 'auto-mode-alist '("^BUILD.bazel$" . python-mode))
(add-to-list 'auto-mode-alist '("^BUILD$" . python-mode))
(add-to-list 'auto-mode-alist '("^WORKSPACE$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'". dockerfile-mode))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(toggle-scroll-bar nil)
(setq split-height-threshold 1) ;default to split windows horizontally

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 2)))

(add-hook 'java-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 4)))

(add-hook 'clojure-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 4)))

(add-hook 'ruby-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 2)))

(add-hook 'rjsx-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq js-indent-level 4)
    (prettier-js-mode)))

(add-hook 'js-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq js-indent-level 2)
    (prettier-js-mode)))

(add-hook 'web-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (eldoc-mode 1)
    (company-mode 1)
    (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(add-hook 'yaml-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)))

(add-hook 'nxml-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 2)))

(add-hook 'python-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq python-indent 4)))

(add-hook 'terraform-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)))

(add-hook 'sh-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)))

(add-hook 'dockerfile-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq c-basic-offset 2)))

(add-hook 'text-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (auto-fill-mode)))

;; Don't edit these

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old
(put 'narrow-to-region 'disabled nil)

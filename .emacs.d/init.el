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

;; tabs still don't work correctly
;;(setq-default indent-tabs-mode nil)
;;(setq c-basic-offset 2)
;;(setq tab-width 2)

(set-face-attribute 'default nil :font "Courier New 14")

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.  Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
	   (package-install package)
	 package)))
   packages)

  ;; Make sure to have downloaded archive description
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ;; Activate installed packages
  (package-initialize))

(ensure-package-installed 'ag
			  'helm
			  'helm-projectile
                          'helm-ag
                          'evil
                          'evil-leader
                          'evil-indent-textobject
                  	  'projectile
                          'magit
                          'zenburn-theme
			  'powerline
			  'exec-path-from-shell
                          'php-mode
                          'php-extras)
 
(load-theme 'zenburn t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f34b107e8c8443fe22f189816c134a2cc3b1452c8874d2a4b2e7bb5fe681a10b" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (powerline-evil org-bullets powerline swiper-helm swiper counsel exec-path-from-shell helm-ag ag use-package-chords use-package php-extras magit helm-spotify helm-projectile evil-visual-mark-mode evil-leader evil-indent-textobject)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(exec-path-from-shell-initialize)

(toggle-frame-maximized)
(global-linum-mode 1)

;; Load in new configs
(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
(require 'init-evil)
(require 'init-org)
(require 'init-powerline)

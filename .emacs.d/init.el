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
                  	  'projectile
                          'magit
                          'php-mode
                          'php-extras)

(global-linum-mode 1)
(set-face-attribute 'default nil :font "Courier New 14")
(toggle-frame-maximized)

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.  Listed alphabetically
(require 'init-evil)
(require 'init-exec-path-from-shell)
(require 'init-org)
(require 'init-powerline)
(require 'init-zenburn-theme)

;;; Commentary:
;;; package -- summary
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(pbl--profile "init-header")
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

(setq inhibit-startup-message t)

(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq-default fill-column 80)
; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
; (setq inferior-lisp-program "sbcl")

(let ((private-settings (expand-file-name "~/private.el")))
  (if (file-exists-p private-settings)
      (load private-settings)))

;; Visual presentation of window
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
 '(package-selected-packages
   '(let-alist flycheck-mode helm-rg minimap visual-fill-column writeroom-mode prettier-js w3m w3 telephone-line spotify clojure-mode unicode-fonts flow-minor-mode flow-mode flycheck-yamllint flycheck dockerfile-mode puppet-mode yaml-mode company zenburn-theme powerline-evil powerline org-bullets magit exec-path-from-shell evil-indent-textobject evil-leader evil php-mode helm-projectile helm use-package)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)
(pbl--profile "init-header")

(pbl--profile "use-package")
(eval-when-compile
  (require 'use-package))

(use-package diminish :defer 1)
(use-package bind-key :defer 1)
(use-package rainbow-delimiters :defer 1)

(use-package prettier-js :defer 1)
(use-package helm :defer 1
  :diminish helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

; per helm-projectile.el comments, helm-projectile-fuzzy-match needs to load before helm-projectile package
(setq helm-projectile-fuzzy-match nil)
(use-package helm-projectile :defer t)
(use-package projectile
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))
(use-package yaml-mode :defer 1)
(use-package web-mode :defer 1)
(use-package terraform-mode :defer 1)
(use-package dockerfile-mode :defer 1)
(use-package rjsx-mode :defer 1)
(pbl--profile "use-package")

(pbl--profile "init-config")
(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.

(require 'init-exec-path-from-shell)
(require 'init-evil)
(require 'init-zenburn-theme)
(require 'init-telephone-line)
(use-package init-company :defer 1)
;(use-package init-magit :defer 1)
(use-package init-writeroom-mode :defer 1)
(use-package init-minimap :defer 1)
(use-package init-helm-rg :defer 1)
(pbl--profile "init-config")

(pbl--profile "init-org")
(require 'init-org)
(pbl--profile "init-org")
(pbl--profile "org-agenda")
(org-agenda nil "c") ; load org-agenda
(pbl--profile "org-agenda")

(pbl--profile "mode-hooks")
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

(add-hook 'rjsx-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq js-indent-level 4) ;@TODO: Can I get this from configs?
    (prettier-js-mode)
    (company-mode 1)
    (eldoc-mode)
    (flycheck-mode 1)
    ))

(add-hook 'js-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq js-indent-level 2)
    (prettier-js-mode)
    (flycheck-mode 1)))

(add-hook 'web-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (flycheck-mode 1)))

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

(add-hook 'writeroom-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))
(pbl--profile "mode-hooks")

; Don't edit these

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

(defvar pbl--init-profile-buffer-name "*init-profile*")
(defvar pbl--init-profile-header-line "Init Profiler")
(defvar pbl--right-pad-size 15)

(defun format-sec (after-time before-time)
  (format "%.2fs" (float-time (time-subtract after-time before-time))))

(defun pbl--right-pad-val (val &optional total-size)
  "Pad val up to TOTAL-SIZE."
  (let* ((val-string (if (numberp val) (number-to-string val) val))
         (cols (if total-size total-size pbl--right-pad-size))
         (num-spaces (max 0 (- cols (length val-string))))
         (spaces (make-string num-spaces ?\ )))
      (concat val-string spaces)))

(defun pbl--display-init-profile-results ()
  "Parse and display list in window"
  (with-current-buffer (pbl--prepare-init-profile-buffer pbl--init-profile-buffer-name)
    (setq buffer-read-only nil)
    (setq header-line-format pbl--init-profile-header-line)
    (pbl--display-init-profile-data)
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

(defun pbl--display-init-profile-data () ""
       (mapcar (lambda (elem)
                 (let* ((key (car elem))
                        (ts (cdr elem))
                        (before (car ts))
                        (after (cadr ts)))
                   (insert (concat (pbl--right-pad-val key) (format-sec after before) "\n"))))
               pbl--profile-times)
       (insert (concat (pbl--right-pad-val "total init") (format-sec after-init-time before-init-time) "\n"))
       (insert (concat (pbl--right-pad-val "gc") (format "%d" gcs-done))))

;;@TODO: Add "q" to exit buffer
(defun pbl--prepare-init-profile-buffer (buffer-name &optional mode-cb)
  "Create consistent buffer object for displaying list items"
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (when mode-cb (funcall mode-cb))
      (setq buffer-read-only t))
    buf))

(add-hook 'emacs-startup-hook
          (lambda ()
            (pbl--display-init-profile-results)))

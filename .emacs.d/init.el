;;; Commentary:
;;; package -- summary
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
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
   '(init-minimap init-magit treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs doom-modeline org flycheck-mode minimap visual-fill-column writeroom-mode prettier-js clojure-mode unicode-fonts flow-minor-mode flow-mode flycheck-yamllint flycheck dockerfile-mode puppet-mode yaml-mode company zenburn-theme powerline-evil powerline org-bullets magit evil-indent-textobject evil-leader evil php-mode use-package)))
 ;'(prettier-js-command "/usr/local/bin/prettier"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package))

(use-package diminish :defer 1)
(use-package bind-key :defer 1)
(use-package rainbow-delimiters :ensure t)

(use-package prettier-js :ensure t)
(use-package ivy :ensure t)
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)

(use-package projectile
  :defer 1
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))
(use-package yaml-mode :ensure t :defer 1)
(use-package web-mode :ensure t :defer 1)
(use-package terraform-mode :ensure t :defer 1)
(use-package dockerfile-mode :ensure t :defer 1)
(use-package rjsx-mode :ensure t :defer t)

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(require 'init-evil)
(require 'init-zenburn-theme)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package init-company :defer 1)
(use-package init-magit :defer 1)
(use-package init-writeroom-mode :defer 1)
(use-package init-minimap :defer 1)
(use-package init-flycheck :defer 1)

(require 'init-org)
(org-agenda nil "c") ; load org-agenda

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
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
    (flycheck-mode 1)))

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
(defvar pbl--right-pad-size 30)

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
                        (after (cadr ts))
                        (duration (float-time (time-subtract after before))))
                   (when (>= duration 0.05)
                     (insert (concat (pbl--right-pad-val key) (format "%.2fs" duration) "\n")))))
               pbl--profile-times)
       (insert (concat (pbl--right-pad-val "total init") (format "%.2fs" (float-time (time-subtract after-init-time before-init-time))) "\n"))
       (insert (concat (pbl--right-pad-val "gc") (format "%d" gcs-done))))

;;@TODO: Add "q" to exit buffer
;; jk just going to bind ,q in evil-mode
(defun pbl--prepare-init-profile-buffer (buffer-name)
  "Create consistent buffer object for displaying list items"
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (setq buffer-read-only t))
    buf))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs Init: %.2fs. GC: %d."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;  :after treemacs persp-mode ;;or perspective vs. persp-mode
;  :ensure t
;  :config (treemacs-set-scope-type 'Perspectives))

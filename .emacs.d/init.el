;;; Commentary:
;;; package -- summary
;;; Code:

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(pbl--profile "start")
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

;(let ((private-settings (expand-file-name "~/private.el")))
;  (if (file-exists-p private-settings)
;      (load private-settings)))

;; Visual presentation of window
(global-visual-line-mode 1)

(setq create-lockfiles nil)
(setq vc-follow-symlinks t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline org visual-fill-column writeroom-mode clojure-mode unicode-fonts flow-minor-mode flow-mode company zenburn-theme org-bullets evil-indent-textobject evil-leader evil use-package)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)

(eval-when-compile
  (require 'use-package))

(use-package diminish :defer 1)
(use-package bind-key :defer 1)
(use-package rainbow-delimiters :ensure t)

(use-package ivy :ensure t :defer 1)

(add-to-list 'load-path (expand-file-name "init-config" user-emacs-directory))
;; Additional configs to load.
(pbl--profile "start")

(pbl--profile "init-evil")
(require 'init-evil)
(pbl--profile "init-evil")

(pbl--profile "init-zenburn-theme")
(require 'init-zenburn-theme)
(pbl--profile "init-zenburn-theme")

(pbl--profile "doom-modeline")
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(pbl--profile "doom-modeline")

(pbl--profile "init-writeroom-mode")
(use-package init-writeroom-mode :defer 1)
(pbl--profile "init-writeroom-mode")

(pbl--profile "init-org")
(require 'init-org)
(pbl--profile "init-org")
(pbl--profile "org setup")
(org-agenda nil "c") ; load org-agenda
(pbl--profile "org setup")

(pbl--profile "set values")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(toggle-scroll-bar nil)
(setq split-height-threshold 1) ;default to split windows horizontally
(pbl--profile "set values")

(pbl--profile "hooks")
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (display-line-numbers-mode 1)
    (eldoc-mode 1)
    (company-mode 1)
    (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(add-hook 'text-mode-hook
          (lambda ()
            (display-line-numbers-mode 1)
            (auto-fill-mode)))

(add-hook 'writeroom-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))
(pbl--profile "hooks")

(pbl--profile "emacs stuff I didn't make")
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
(pbl--profile "emacs stuff I didn't make")

(pbl--profile "profiler I made")
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
                   ;(when (>= duration 0.05)
                     ;(insert (concat (pbl--right-pad-val key) (format "%.2fs" duration) "\n")))))
                     (insert (concat (pbl--right-pad-val key) (format "%.2fs" duration) "\n"))))
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
            (pbl--display-init-profile-results)))
;            (message "Emacs Init: %.2fs. GC: %d."
;                     (float-time (time-subtract after-init-time before-init-time))
;                     gcs-done)))

(pbl--profile "profiler I made")

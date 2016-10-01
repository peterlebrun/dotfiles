(defun my-php-lineup-arglist-intro (langelem)
  "Align PHP argument list intro based on LANGELEM."
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-arglist-close (langelem)
  "Align PHP argument list close based on LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (current-column))))

(defun my-php-lineup-arglist-cont-nonempty (langelem)
  "Align continued arglist lines to two times the basic offset from LANGELEM."
  (save-excursion
    (goto-char (c-langelem-pos langelem))
    (vector (+ (current-column) (* 2 c-basic-offset)))))

(defun my-php-lineup-statement-cont (langelem)
  "Align PHP continued statements based on LANGELEM."
  (message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (save-excursion
    (back-to-indentation)
    (if (search-forward "->" (line-end-position) t)
	(* 2 c-basic-offset)
      (php-lineup-string-cont langelem))))

(defun configure-php-mode ()
  "Set up PHP mode preferences"
  (require 'newcomment)
  (setq comment-auto-fill-only-comments 1)
  (setq auto-fill-function 'do-auto-fill)
  (setq flycheck-disabled-checkers '(php-phpmd))

  (when (boundp 'company-backends)
    (setq-local company-backends
		'((company-dabbrev-code php-extras-company))))

  (when (fboundp 'php-extras-eldoc-documentation-function)
    (add-function :before-until (local 'eldoc-documentation-function)
		  'php-extras-eldoc-documentation-function))

  (pbl-set-php-group)
  (eldoc-mode t)
  (highlight-symbol-mode)
  (electric-pair-mode)
  (turn-on-auto-fill)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  (flycheck-mode))

(defvar php-settings-groups
  '(("air"
     (lambda ()
       ;; default PHP style.
       (c-set-style "php")
       (setq-local flycheck-phpcs-standard "PSR2")
       (set-fill-column 85)))
    ("wf"
     (lambda ()
       (c-add-style "wf-php"
		    '("php"
		      (c-basic-offset . 2)
		      (c-offsets-alist . ((arglist-intro . my-php-lineup-arglist-intro)
					  (arglist-close . my-php-lineup-arglist-close)
					  (arglist-cont-nonempty . my-php-lineup-arglist-cont-nonempty)
					  (statement-cont . my-php-lineup-statement-cont)
					  (topmost-intro-cont . my-php-lineup-statement-cont)))))
       (c-add-style "wf-php")
       (setq-local flycheck-phpcs-standard "CSNStores")
       (set-fill-column 120))))
  "Groups of PHP settings")

(defun air-set-php-group (&optional php-settings-group)
  "BLEH"
  (let* ((group-file (expand-file-name "php-settings-group" user-emacs-directory))
	 (group (or php-settings-group
		    (and (file-readable-p group-file)
			 (with-temp-buffer
			   (insert-file-contents group-file)
			   (buffer-substring (point-min) (point-max))))
		    (nth 0 php-settings-groups)))
	 (settings (assoc group php-settings-groups)))
    (ignore-errors (funcall (cadr settings)))))

(use-package php-mode
  :mode "\\.php\\"
  :config
  (add-hook 'php-mode-hook 'configure-php-mode))

(provide 'init-php)

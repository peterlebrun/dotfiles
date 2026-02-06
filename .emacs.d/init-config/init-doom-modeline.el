;(use-package doom-themes)
;(use-package keycast
  ;:config
  ;;; This works with doom-modeline, inspired by this comment:
  ;;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  ;(define-minor-mode keycast-mode
    ;"Show current command and its key binding in the mode line."
    ;:global t
    ;(if keycast-mode
        ;(add-hook 'pre-command-hook 'keycast-mode-line-update t)
        ;(remove-hook 'pre-command-hook 'keycast-mode-line-update)))
  ;(add-to-list 'global-mode-string '("" mode-line-keycast " "))
  ;(keycast-mode))
(use-package nerd-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "NFM.ttf" "~/Library/Fonts/"))
    (nerd-icons-install-fonts t)))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (column-number-mode)
  (display-time-mode))
(use-package all-the-icons-nerd-fonts :ensure t)

(provide 'init-doom-modeline)

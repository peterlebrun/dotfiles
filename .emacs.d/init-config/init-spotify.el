;; I really wish this package got into melpa...
(add-to-list 'load-path (concat pbl-package-root "spotify.el/"))

(setq spotify-mode-line-truncate-length 8)
(setq spotify-mode-line-format "[%p %a: %t %R%S]")

(setq spotify-mode-line-playing-text "▶")
(setq spotify-mode-line-paused-text "‖")
(setq spotify-mode-line-stopped-text "x")

;; @TODO: This is going to cause problems if emacs starts
;; without a network connection
(require 'spotify)

(provide 'init-spotify)

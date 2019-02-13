;; I really wish this package got into melpa...
(add-to-list 'load-path (concat pbl-package-root "spotify.el/"))
;; @TODO: This is going to cause problems if emacs starts
;; without a network connection
(setq spotify-mode-line-truncate-length 10)
(require 'spotify)

(provide 'init-spotify)

;(use-package doom-themes)
(use-package keycast
  :ensure t
  :init
  (keycast-mode-line-mode))
(column-number-mode)
(display-time-mode)
;(use-package doom-modeline
  ;:ensure t
  ;:init
  ;(doom-modeline-mode 1)
  ;(column-number-mode)
  ;(display-time-mode))

(provide 'init-doom-modeline)

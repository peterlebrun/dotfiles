(require 'ob-python)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-babel-python-command "python3")

(provide 'init-org-babel)

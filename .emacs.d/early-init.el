;; -*- lexical-binding: t -*-
;; @TODO: This will behave weirdly if it is called >2x with the same key, add safeguards
(defun pbl--profile (key)
  (when (not (boundp 'pbl--profile-times)) (setq pbl--profile-times ()))
  (let* ((c-time (current-time))
         (cur-cons (if (boundp 'pbl--profile-times)
                       (alist-get key pbl--profile-times nil nil 'equal)
                     nil)))
    (if cur-cons
        (setf (alist-get key pbl--profile-times nil nil 'equal) (append cur-cons `(,c-time)))
      (setf (alist-get key pbl--profile-times nil nil 'equal) `(,c-time)))))

(pbl--profile "early-init")

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Set default font
(set-face-attribute 'default nil :family "Fira Code"
                    :height 180
                    :weight 'medium
                    :width 'normal)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(pbl--profile "early-init")

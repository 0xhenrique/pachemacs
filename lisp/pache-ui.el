;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Add more themes here
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))

;; Mode line settings
;(display-battery-mode 1)
;(setq display-time-day-and-date t)
;(display-time-mode 1)

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set the default theme
(defun pache/random-theme ()
  "Load a random theme from a predefined list of themes."
  (let ((themes '(catppuccin gruvbox-dark-hard modus-vivendi)))
    (load-theme (nth (random (length themes)) themes) t)))
(add-hook 'emacs-startup-hook 'pache/random-theme)

;; Some icons for Emacs
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))
(use-package all-the-icons
  :if (display-graphic-p))

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Some other visual tweaks
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq scroll-conservatively 100
      resize-mini-windows 'grow-only)

(provide 'pache-ui)
;;; pache-ui.el ends here

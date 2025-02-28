;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Themes
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))
(unless (package-installed-p 'ef-themes)
  (package-install 'ef-themes))

(load-theme 'ef-duo-dark)

;; Icons
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(unless (package-installed-p 'all-the-icons-dired)
  (package-install 'all-the-icons-dired))
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set a random theme on startup
(defun pache/random-theme ()
  "Load a random theme from a predefined list of themes."
  (let ((themes '(catppuccin gruvbox-dark-hard modus-vivendi)))
    (load-theme (nth (random (length themes)) themes) t)))
;;(add-hook 'emacs-startup-hook 'pache/random-theme)

(use-package all-the-icons
  :if (display-graphic-p))

(setq visible-bell t
      ring-bell-function t
      scroll-conservatively 100
      resize-mini-windows 'grow-only)

;; Some other visual tweaks
(all-the-icons-dired-mode t)
(rainbow-delimiters-mode t)
(fringe-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'pache-ui)
;;; pache-ui.el ends here

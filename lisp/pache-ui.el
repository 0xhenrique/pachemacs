;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))
(unless (package-installed-p 'ef-themes)
  (package-install 'ef-themes))
(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(unless (package-installed-p 'all-the-icons-dired)
  (package-install 'all-the-icons-dired))
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))
(unless (package-installed-p 'rainbow-mode)
  (package-install 'rainbow-mode))
(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(load-theme 'gruvbox-dark-hard t)

;; Frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-hook 'emacs-startup-hook 'pache/random-theme)

(use-package all-the-icons
  :if (display-graphic-p))

(setq visible-bell t
      ring-bell-function t
      scroll-conservatively 100
      resize-mini-windows 'grow-only)

;; Some other visual tweaks
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(global-display-line-numbers-mode 0)
(global-hl-line-mode t)
(doom-modeline-mode)

;;(fringe-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'pache-ui)
;;; pache-ui.el ends here

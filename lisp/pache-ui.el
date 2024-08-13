;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; You can add more themes here
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))

;; Set the default theme
(load-theme 'catppuccin t)

;; Some icons for Emacs
(use-package all-the-icons
  :if (display-graphic-p))

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Comfy")

;; Dashboard settings
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/img/pache-1.png")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents . 5)
  			  (bookmarks . 5)
  			  (projects . 5)
  			  (agenda . 5))))

;; Some other visual tweaks
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
(setq scroll-conservatively 100
      display-time-default-load-average nil)

(provide 'pache-ui)
;;; pache-ui.el ends here

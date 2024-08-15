;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; You can add more themes here
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))

;; Set the default theme
(load-theme 'gruvbox-dark-hard t)

;; Some icons for Emacs
(unless (package-installed-p 'nerd-icons)
  (package-install 'nerd-icons))
(use-package all-the-icons
  :if (display-graphic-p))

;; Install Doom Modeline
(unless (package-installed-p 'doom-modeline)
  (package-install 'doom-modeline))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Comfy")

;; Dashboard settings
;; (unless (package-installed-p 'dashboard)
;;   (package-install 'dashboard))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner "~/.emacs.d/img/pache-5.png"
;; 	dashboard-banner-logo-title "P A C H E M A C S"
;; 	dashboard-footer-messages '("\"The shrine isn't a good place for using magic.\"")
;; 	dashboard-center-content t
;; 	dashboard-vertically-center-content t
;; 	dashboard-items '((recents . 8)
;; 			  (bookmarks . 5)
;; 			  (projects . 3)
;; 			  (agenda . 3))))

;; Some other visual tweaks
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
(setq scroll-conservatively 100
      resize-mini-windows 'grow-only
      display-time-default-load-average nil)

(provide 'pache-ui)
;;; pache-ui.el ends here

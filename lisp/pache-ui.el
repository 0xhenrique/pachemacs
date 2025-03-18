;;; pache-ui.el --- UI Tweaks -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(dolist (pkg '(gruvbox-theme
               catppuccin-theme
               ef-themes
               all-the-icons
               all-the-icons-dired
               nerd-icons
               rainbow-mode
               doom-modeline))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(load-theme 'pache-dark t)

;; Frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

;; Org heading sizes.
;(set-face-attribute 'org-document-title nil :height 2.0)
;(set-face-attribute 'org-level-1 nil :height 1.6)
;(set-face-attribute 'org-level-2 nil :height 1.4)
;(set-face-attribute 'org-level-3 nil :height 1.2)
;(set-face-attribute 'org-level-4 nil :height 1.0)

(provide 'pache-ui)
;;; pache-ui.el ends here

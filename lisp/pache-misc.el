;;; pache-misc.el --- Everything that doesn't fit the other modules -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(dolist (pkg '(editorconfig
			   evil
			   evil-collection
			   which-key
			   magit
			   ivy
			   sudo-edit
			   counsel
			   drag-stuff
			   yasnippet
			   ;; UI
               doom-modeline
			   ;; Programming
			   json-mode
			   elixir-mode
			   rust-mode
			   typescript-mode
			   vue-mode
			   flycheck))
  (unless (package-installed-p pkg)
	(package-install pkg)))

(load-theme 'pache-dark t)
;(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq evil-want-keybinding nil)
(evil-collection-init '(dired magit))
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(add-hook 'prog-momde-hook #'evil-local-mode)

(setq-default tab-width 4
			  standard-indent 4
			  electric-indent-inhibit t
			  indent-tabs-mode t)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t
	  backward-delete-char-untabify-method 'nil
      indent-line-function 'insert-tab
      global-auto-revert-non-file-buffers t
      ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      counsel-find-file-at-point t
	  ;; UI
	  visible-bell t
      ring-bell-function t
      scroll-conservatively 100
      resize-mini-windows 'grow-only
	  )

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")

(use-package yasnippet
  :ensure
  :bind
  (:map yas-minor-mode-map
        ("C-'". yas-expand)
        ([(tab)] . nil)
        ("TAB" . nil))
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Import programming languages specifics ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/lisp/languages/go.el")
(load "~/.emacs.d/lisp/languages/typescript.el")
(load "~/.emacs.d/lisp/languages/vue.el")
;(load "~/.emacs.d/lisp/languages/rust.el")

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
(electric-pair-mode t)
(company-mode t)
(global-display-line-numbers-mode 0)
(global-hl-line-mode t)
(doom-modeline-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode)
(multiple-cursors-mode)
(which-key-mode)
(counsel-mode)
(ido-mode)
(ivy-mode)
(save-place-mode t)
(flymake-mode 0)
(savehist-mode t)
(recentf-mode t)
(editorconfig-mode)
(drag-stuff-global-mode t)
(global-display-line-numbers-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Org heading sizes. ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;(set-face-attribute 'org-document-title nil :height 2.0)
;(set-face-attribute 'org-level-1 nil :height 1.6)
;(set-face-attribute 'org-level-2 nil :height 1.4)
;(set-face-attribute 'org-level-3 nil :height 1.2)
;(set-face-attribute 'org-level-4 nil :height 1.0)

(provide 'pache-misc)
;;; pache-misc.el ends here

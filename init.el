;;; init --- personal init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Common modules for Pachemacs
;; (add-to-list 'load-path "/home/arisu/.emacs.d/lisp/")
;; If you're not an EXWM enjoyer, you can remove or comment the line below
(load "~/.emacs.d/lisp/pache-exwm.el")
(load "~/.emacs.d/lisp/pache-ui.el")
;; If you're not an Evil enjoyer, you might also want to remove pache-evil below
(load "~/.emacs.d/lisp/pache-evil.el")
(load "~/.emacs.d/lisp/pache-keys.el")
(load "~/.emacs.d/lisp/pache-misc.el")
(load "~/.emacs.d/lisp/pache-yas.el")
(load "~/.emacs.d/lisp/pache-programming.el")
(load "~/.emacs.d/lisp/pache-irc.el")

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      scroll-step 1
      sentence-end-double-space nil
      backup-directory-alist `(("." . "~/.saves"))
      inhibit-startup-echo-area-message "The shrine isn't a good place for using magic.")

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(global-display-line-numbers-mode t)
;; (setq visible-bell 1)

;;; Make Emacs run faster
(setq gc-cons-threshold 100000000
      read-process-output-max (* (* 1024 1024) 3)
      inhibit-compacting-font-caches t)

(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; Disable some LSP options if Emacs gets too slow
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-log-io nil
;;         lsp-idle-delay 0.5
;;         lsp-auto-guess-root nil
;;         lsp-restart 'auto-restart
;;         lsp-enable-symbol-highlighting nil
;;         lsp-enable-on-type-formatting nil
;;         lsp-signature-auto-activate nil
;;         lsp-signature-render-documentation nil
;;         lsp-eldoc-hook nil
;;         lsp-modeline-code-actions-enable nil
;;         lsp-modeline-diagnostics-enable nil
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-semantic-tokens-enable nil
;;         lsp-enable-folding nil
;;         lsp-enable-imenu nil
;;         lsp-enable-snippet nil))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a060c0315a44bee19ac87414571131c8c8fad7a940bda0102da67410e66833aa" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" default))
 '(package-selected-packages
   '(helm openwith geiser-guile geiser nix-mode counsel swiper exwm yasnippet yaml-mode which-key web-mode vue-mode vertico typescript-mode sudo-edit solarized-theme sly rust-mode php-mode nvm nerd-icons multi-vterm move-text magit lua-mode lsp-mode lemon json-mode ivy gruvbox-theme go-mode flycheck evil-mc emms editorconfig eat diff-hl corfu consult catppuccin-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

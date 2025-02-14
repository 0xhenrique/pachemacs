;;; init --- personal init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Modules for Pachemacs
;;(load "~/.emacs.d/lisp/pache-exwm.el")
(load "~/.emacs.d/lisp/pache-ui.el")
(load "~/.emacs.d/lisp/pache-win.el")
(load "~/.emacs.d/lisp/pache-evil.el")
(load "~/.emacs.d/lisp/pache-keys.el")
(load "~/.emacs.d/lisp/pache-misc.el")
(load "~/.emacs.d/lisp/pache-utils.el")
(load "~/.emacs.d/lisp/pache-yas.el")
(load "~/.emacs.d/lisp/pache-programming.el")
(load "~/.emacs.d/lisp/pache-irc.el")
(load "~/.emacs.d/lisp/pache-media.el")

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      visual-line-mode t
      scroll-step 1
      ring-bell-function 'ignore
      visible-bell 1
      inhibit-startup-screen t
      sentence-end-double-space nil
      backup-directory-alist `(("." . "~/.saves")))

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(global-display-line-numbers-mode t)

;;; Emacs Perf
(setq gc-cons-threshold 100000000
      read-process-output-max (* (* 1024 1024) 3)
      inhibit-compacting-font-caches t)
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; Disable some LSP options if Emacs gets too slow
;;
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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c21904759f8d6d73f8be4a03c71a81b9908e71276c490f664022bf997111b458" "75b371fce3c9e6b1482ba10c883e2fb813f2cc1c88be0b8a1099773eb78a7176" "18a1d83b4e16993189749494d75e6adb0e15452c80c431aca4a867bcc8890ca9" "2f9cff368c07d280a7a766e9f04a0053a17bb74f775504dc49421d1fda2a0797" "deb645f30fd25191b6e8d0f397cc1dd172a352f22094747be2ff527394cc9f57" "f1c8202c772d1de83eda4765fe21429a528a4fb350a28394d3705fe9678ed1f9" "b1162ee87ca94024dbb677dc40c8d8e5ec02d3ccf505bed683f4aa11604468d0" "0a2168af143fb09b67e4ea2a7cef857e8a7dad0ba3726b500c6a579775129635" "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3" "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87" "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378" default))
 '(package-selected-packages
   '(mentor grayscale-theme copilot gptel solarized-theme nofrils-acme-theme acme-theme plan9-theme desktop-environment exwm yasnippet yaml-mode which-key web-mode vue-mode vertico typescript-mode sudo-edit sly rust-mode rainbow-delimiters php-mode openwith nvm multi-vterm move-text magit lua-mode lsp-mode json-mode helm gruvbox-theme go-mode geiser-guile flycheck evil-mc emms editorconfig eat doom-modeline diff-hl counsel corfu consult catppuccin-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

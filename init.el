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
      scroll-step 1
      ring-bell-function 'ignore
      visible-bell 1
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

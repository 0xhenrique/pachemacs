;;; init --- personal init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Common modules for Pachemacs
(add-to-list 'load-path "~/.emacs.d/lisp")
;; If you're not an EXWM enjoyer, you can remove or comment the line below
(require 'pache-ui)
(require 'pache-exwm)
;; If you're not an Evil enjoyer, you might also want to remove pache-evil below
(require 'pache-evil)
(require 'pache-misc)
(require 'pache-yas)
(require 'pache-programming)
(require 'pache-irc)

(when (file-exists-p custom-file)
  (load custom-file))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq confirm-kill-emacs #'yes-or-no-p
      display-line-numbers-type 'relative
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      scroll-step 1
      sentence-end-double-space nil
      custom-file (locate-user-emacs-file "custom.el")
      backup-directory-alist `(("." . "~/.saves"))
      inhibit-startup-echo-area-message "The shrine isn't a good place for using magic."
      )

(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)
(global-display-line-numbers-mode t)

;;; Make Emacs run faster
(setq gc-cons-threshold 100000000
      read-process-output-max (* (* 1024 1024) 3)
      run-with-idle-timer 2 t (lambda () (garbage-collect))
      lsp-log-io nil
      lsp-idle-delay 1
      lsp-auto-guess-root t
      lsp-restart 'auto-restart
      lsp-enable-symbol-highlighting nil
      lsp-enable-on-type-formatting nil
      lsp-signature-auto-activate nil
      lsp-signature-render-documentation nil
      lsp-eldoc-hook nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-headerline-breadcrumb-enable t
      lsp-semantic-tokens-enable nil
      lsp-enable-folding nil
      lsp-enable-imenu nil
      lsp-enable-snippet nil
      read-process-output-max (* (* 1024 1024) 3)
      lsp-idle-delay 0.5
      )

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(provide 'init)
;;; init.el ends here

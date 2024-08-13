;;; init --- personal init -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;; Enable MELPA
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(ido-mode t)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; If you're not an EXWM enjoyer, you can remove or comment the line below
(require 'pache-ui)
(require 'pache-exwm)
(require 'pache-evil)
(require 'pache-misc)
(require 'pache-yas)
(require 'pache-programming)

;; Enable line numbering by default
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Some custom options
(setq
 auto-save-default nil
 scroll-step 1
 sentence-end-double-space nil
 backup-directory-alist `(("." . "~/.saves"))
 inhibit-startup-echo-area-message "I'm confused again"
 )

;;; IRC stuff
;; Set nickname & real-name as constant variables
(setq
 erc-nick "HenriqMarq"     ; Our IRC nick
 erc-user-full-name "Henrique Marques") ; Our /whois name

;;; Some other tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* (* 1024 1024) 3)) ;; 3mb
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq lsp-log-io nil) ; if set to true can cause a performance hit
(setq lsp-idle-delay 1)
(setq lsp-auto-guess-root t)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-hook nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil)
(setq lsp-enable-snippet nil)
(setq read-process-output-max (* (* 1024 1024) 3)) ;; 3MB
(setq lsp-idle-delay 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(provide 'init)
;;; init.el ends here
;; (put 'dired-find-alternate-file 'disabled nil)

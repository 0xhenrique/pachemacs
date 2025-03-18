;;; init --- personal init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Modules for Pachemacs
;(load "~/.emacs.d/lisp/pache-meow.el")
;(load "~/.emacs.d/lisp/pache-irc.el")
;(load "~/.emacs.d/lisp/pache-win.el")
;(load "~/.emacs.d/lisp/pache-ai.el")
;(load "~/.emacs.d/lisp/pache-exwm.el")
(load "~/.emacs.d/lisp/pache-ui.el")
(load "~/.emacs.d/lisp/pache-evil.el")
(load "~/.emacs.d/lisp/pache-keys.el")
(load "~/.emacs.d/lisp/pache-misc.el")
(load "~/.emacs.d/lisp/pache-utils.el")
(load "~/.emacs.d/lisp/pache-yas.el")
(load "~/.emacs.d/lisp/pache-programming.el")
(load "~/.emacs.d/lisp/pache-media.el")
(load "~/.emacs.d/lisp/pache-blog.el")

(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      visual-line-mode nil
      custom-file (locate-user-emacs-file "custom-vars.el")
      context-menu-mode t
      scroll-step 1
      ring-bell-function 'ignore
      visible-bell 1
      inhibit-startup-screen t
      sentence-end-double-space nil
      backup-directory-alist `(("." . "~/.saves")))

(load custom-file 'noerror 'nomessage)
(save-place-mode t)
(flymake-mode 0)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;;; Emacs Perf
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq gc-cons-threshold 100000000
      read-process-output-max (* (* 1024 1024) 3)
      inhibit-compacting-font-caches t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)

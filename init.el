;;; init --- personal init -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Modules for Pachemacs
;(load "~/.emacs.d/lisp/pache-irc.el")
(load "~/.emacs.d/lisp/pache-misc.el")
(load "~/.emacs.d/lisp/pache-exwm.el")
(load "~/.emacs.d/lisp/pache-keys.el")
(load "~/.emacs.d/lisp/pache-utils.el")
(load "~/.emacs.d/lisp/pache-blog.el")

;;(set-frame-font "Aporetic Sans Mono 11")

(load-file "~/workspace/0xhenrique/esb/esb.el")
(setq epa-pinentry-mode 'loopback)
(setq epa-file-select-keys nil)
(setq esb-bookmarks-file "~/workspace/0xhenrique/bookmarks/bookmarks.gpg")

(global-set-key (kbd "C-c e s") 'esb-select-bookmark)
(global-set-key (kbd "C-c e a") 'esb-add-bookmark)
(global-set-key (kbd "C-c e d") 'esb-delete-bookmark)
(global-set-key (kbd "C-c e l") 'esb-list-bookmarks)
(global-set-key (kbd "C-c e e") 'esb-edit-bookmark)
(global-set-key (kbd "C-c e r") 'esb-reload-bookmarks)
(global-set-key (kbd "C-c e i") 'esb-initialize)
(global-set-key (kbd "C-c e f") 'esb-flush-cache)

(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      visual-line-mode nil
      custom-file (locate-user-emacs-file "~/workspace/dump/custom-vars.el")
      context-menu-mode t
      scroll-step 1
      ring-bell-function 'ignore
      visible-bell 1
      inhibit-startup-screen t
      sentence-end-double-space nil
      backup-directory-alist `(("." . "~/.saves")))

(load custom-file 'noerror 'nomessage)
(defalias 'yes-or-no #'y-or-n-p)

;;; Emacs Perf
;;(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq gc-cons-threshold 100000000
      read-process-output-max (* (* 1024 1024) 3)
      inhibit-compacting-font-caches t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)

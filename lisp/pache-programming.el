;;; pache-programming.el --- LSP, programming modes etc -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(dolist (pkg '(json-mode elixir-mode rust-mode typescript-mode vue-mode flycheck))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Import programming languages specifics
(load "~/.emacs.d/lisp/languages/go.el")
(load "~/.emacs.d/lisp/languages/typescript.el")

;; Common programming setup
(defun pache/prog-mode-setup ()
  "Programming modes setup."
  (company-mode)
  (electric-pair-mode)
  (unless (derived-mode-p 'vue-mode 'json-mode 'emacs-lisp-mode)
    (eglot-ensure)))

;; Hooks
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'pache/prog-mode-setup)

(provide 'pache-programming)
;;; pache-programming.el ends here

;;; pache-programming.el --- LSP, programming modes etc -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; LSP Bridge
;(add-to-list 'load-path "~/.emacs.d/lsp-bridge")
;(require 'lsp-bridge)
;(global-lsp-bridge-mode)

;;; Flycheck Support
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; LSP Support with lsp-mode
(use-package lsp-mode
  :init
  (setq
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-delay 0.5
   lsp-ui-doc-show-with-cursor 1)
  :hook (
	 (lsp-mode . (lambda ()
		       (let ((lsp-keymap-prefix "SPC l"))
			 (lsp-enable-which-key-integration))))
	 (vue-mode . lsp)
	 (javascript-mode . lsp)
	 (typescript-mode . lsp)
	 (elixir-mode . lsp)
	 (web-mode . lsp)
	 (clojure-mode . lsp))
  :config
  (define-key evil-normal-state-map (kbd "SPC l") lsp-command-map)
  :commands lsp)

;; LSP Optionals
(use-package lsp-ui :commands lsp-ui-mode)

;; Hooks for programming-mode
(add-hook 'prog-mode-hook #'lsp-ui-mode)
;(add-hook 'prog-mode-hook #'lsp-ui-peek-mode)
(add-hook 'prog-mode-hook #'company-mode)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Programming modes
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))
(unless (package-installed-p 'vue-mode)
  (package-install 'vue-mode))

;; Support for Rust and Cargo
(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

(provide 'pache-programming)
;;; pache-programming.el ends here

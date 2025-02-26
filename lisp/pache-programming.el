;;; pache-programming.el --- LSP, programming modes etc -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; LSP
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

;; Enabled inline static analysis (actually using flycheck right now)
;;(add-hook 'prog-mode-hook #'flymake-mode)

;;; Flycheck Support
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; LSP Support with lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;;(e. g. python-mode)
	 (vue-mode . lsp)
	 (javascript-mode . lsp)
	 (web-mode . lsp)
	 (clojure-mode . lsp)
	 ;; which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; LSP Optionals
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Programming modes
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

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

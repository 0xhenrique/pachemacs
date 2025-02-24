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

;;; Flycheck Support
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; NVM for Node development
;;(unless (package-installed-p 'nvm)
;;  (package-install 'nvm))

;; LSP Support with lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (vue-mode . lsp)
	 (javascript-mode . lsp)
	 (web-mode . lsp)
	 (clojure-mode . lsp)
	 ;; which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
;; optionals
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; PHP Support
(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
  (package-install 'sly))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(unless (package-installed-p 'consult)
  (package-install 'consult))

;; TypeScript, JS, and JSX/TSX support.
;;(use-package web-mode
;;  :ensure t
;;  :mode (("\\.ts\\'" . web-mode)
;;         ("\\.js\\'" . web-mode)
;;         ("\\.mjs\\'" . web-mode)
;;         ("\\.tsx\\'" . web-mode)
;;         ("\\.jsx\\'" . web-mode))
;;  :custom
;;  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;;  (web-mode-code-indent-offset 2)
;;  (web-mode-css-indent-offset 2)
;;  (web-mode-markup-indent-offset 2)
;;  (web-mode-enable-auto-quoting nil))

;; Support for Rust and Cargo
(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

;; YAML
(use-package yaml-mode
  :ensure t)

;; Note that `php-mode' assumes php code is separate from HTML.
;; If you prefer working with PHP and HTML in a single file you
;; may prefer `web-mode'.
(use-package php-mode
  :ensure t)

(provide 'pache-programming)
;;; pache-programming.el ends here

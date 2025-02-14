;;; pache-programming.el --- LSP, programming modes etc -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;;; Vue Support
(unless (package-installed-p 'vue-mode)
  (package-install 'vue-mode))

;;; Flycheck Support
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; NVM for Node development
;;(unless (package-installed-p 'nvm)
;;  (package-install 'nvm))

;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  ;; Add your programming modes here to automatically start Eglot,
  ;; assuming you have the respective LSP server installed.
  :hook ((web-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  ;; You can configure additional LSP servers by modifying
  ;; `eglot-server-programs'. The following tells eglot to use TypeScript
  ;; language server when working in `web-mode'.
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  )

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

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
(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))

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

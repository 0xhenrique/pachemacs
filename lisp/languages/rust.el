;;; rust.el --- Functions and Helpers for Rust/Cargo Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(defun pache/rust-run-clippy ()
  "Run `cargo clippy`."
  (interactive)
  (compile "cargo clippy"))

(defun pache/rust-run-lens ()
  "Run the code lens provided by rust-analyzer."
  (interactive)
  (lsp-execute-code-action-by-kind "refactor"))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("C-c C-a" . 'pache/rust-run-lens)
	      ("C-c C-c" . 'pache/rust-compile)
	      ("C-c C-f" . 'pache/rust-format-buffer)
	      ("C-c C-l" . 'pache/rust-run-clippy)
	      ("C-c C-r" . 'pache/rust-run)
	      ("C-c C-t" . 'pache/rust-test)))

(provide 'rust)
;;; rust.el ends here

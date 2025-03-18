;;; go.el --- Functions and Helpers for Golang Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(defun pache/go-build ()
  "Run `go build` in the project root."
  (interactive)
  (compile "go build ./..."))

(defun pache/go-format ()
  "Format the current Go buffer with gofmt."
  (interactive)
  (when (eq major-mode 'go-mode)
    (save-buffer)
    (shell-command (format "gofmt -w %s" (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(defun pache/go-lint ()
  "Run `golangci-lint run` on the current file."
  (interactive)
  (let ((file (shell-quote-argument (buffer-file-name))))
    (compile (format "golangci-lint run %s" file))))

(defun pache/go-run ()
  "Run `go run .` in the project root."
  (interactive)
  (compile "go run ."))

(defun pache/go-test ()
  "Run `go test ./...`."
  (interactive)
  (compile "go test ./..."))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-b" . pache/go-build)
              ("C-c C-f" . pache/go-format)
              ("C-c C-l" . pache/go-lint)
              ("C-c C-r" . pache/go-run)
              ("C-c C-t" . pache/go-test))
  :hook ((go-mode . eglot-ensure)))

(provide 'go)
;;; go.el ends here

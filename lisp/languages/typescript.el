;;; typescript.el --- Functions and Helpers for Typescript Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(defun pache/typescript-compile ()
  "Run `npm run build` in the project root."
  (interactive)
  (compile "npm run build"))

(defun pache/typescript-lint ()
  "Run `npm run lint` only on the current file."
  (interactive)
  (let ((file (shell-quote-argument (buffer-file-name))))
    (compile (format "npm run lint -- %s" file))))

(defun pache/typescript-test ()
  "Run `npm run test` in the project root."
  (interactive)
  (compile "npm run test"))

(defun pache/typescript-format ()
  "Run `npm run format` in the project root."
  (interactive)
  (compile "npm run format"))

(use-package typescript-mode
  :ensure t
  :bind (:map typescript-mode-map
              ("C-c C-b" . pache/typescript-compile)
              ("C-c C-f" . pache/typescript-format)
              ("C-c C-l" . pache/typescript-lint)
              ("C-c C-t" . pache/typescript-test)))

(provide 'typescript)
;;; typescript.el ends here

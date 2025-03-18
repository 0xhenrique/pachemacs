;;; vue.el --- Functions and Helpers for Vue Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;; Define Vue Mode

(use-package vue-mode
  :ensure t
  :bind (:map vue-mode-map
              ("C-c C-b" . pache/typescript-compile)
              ("C-c C-f" . pache/typescript-format)
              ("C-c C-l" . pache/typescript-lint)
              ("C-c C-t" . pache/typescript-test)))

(provide 'vue)
;;; vue.el ends here

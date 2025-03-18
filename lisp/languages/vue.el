;;; vue.el --- Functions and Helpers for Vue Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:
;; Define Vue Mode

(defun pache/jump-to-section (section)
  "Jump to a specific SECTION in a Vue file."
  (re-search-forward (concat "^<" section ">") nil t))

;(define-key vue-mode-map (kbd "C-c C-j t") (lambda () (interactive) (vue/jump-to-section "template")))
;(define-key vue-mode-map (kbd "C-c C-j s") (lambda () (interactive) (vue/jump-to-section "script")))
;(define-key vue-mode-map (kbd "C-c C-j c") (lambda () (interactive) (vue/jump-to-section "style")))

(use-package vue-mode
  :ensure t
  :config
  (setq mmm-submode-decoration-level 0)
  :bind (:map vue-mode-map
              ("C-c C-b" . pache/typescript-compile)
              ("C-c C-f" . pache/typescript-format)
              ("C-c C-l" . pache/typescript-lint)
              ("C-c C-t" . pache/typescript-test))
  :hook
  ((vue-mode . lsp)))

(provide 'vue)
;;; vue.el ends here

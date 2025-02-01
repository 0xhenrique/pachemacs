;;; pache-yas.el --- YASnippet Settings -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(cond
 ((eq system-type 'gnu/linux)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
 ((eq system-type 'windows-nt)
  (setq yas-snippet-dirs '("C:/snippets"))))

(use-package yasnippet
  :ensure
  :bind
  (:map yas-minor-mode-map
        ("C-'". yas-expand)
        ([(tab)] . nil)
        ("TAB" . nil))
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))
(yas-global-mode 1)

(provide 'pache-yas)
;;; pache-yas.el ends here

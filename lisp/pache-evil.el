;;; pache-evil.el --- Evil Settings -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(unless (package-installed-p 'evil)
  (package-install 'evil))

(evil-mode t)

(require 'evil)
(evil-mode 1)
(define-prefix-command 'my-evil-space-map)
(define-key evil-normal-state-map (kbd "SPC") 'my-evil-space-map)
;; Bind your commands to sequences starting with `SPC` in normal mode
(define-key evil-normal-state-map (kbd "SPC s w") #'consult-ripgrep)
(define-key evil-normal-state-map (kbd "SPC s f") #'project-find-file)
(define-key evil-normal-state-map (kbd "SPC g s") #'magit-status)

;;; Multiple cursors
(unless (package-installed-p 'evil-mc)
  (package-install 'evil-mc))
(global-evil-mc-mode 1)

;; Enable Vim emulation in programming buffers
(add-hook 'prog-mode-hook #'evil-local-mode)

(provide 'pache-evil)
;;; pache-evil.el ends here

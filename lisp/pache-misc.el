;;; pache-misc.el --- Everything that doesn't fit the other modules -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;(unless (package-installed-p 'corfu)
;  (package-install 'corfu))
;(unless (package-installed-p 'helm)
;  (package-install 'helm))
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
(unless (package-installed-p 'geiser-guile)
  (package-install 'geiser-guile))
(unless (package-installed-p 'magit)
  (package-install 'magit))
;(unless (package-installed-p 'vertico)
;  (package-install 'vertico))
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
(unless (package-installed-p 'evil-mc)
  (package-install 'evil-mc))
(unless (package-installed-p 'sudo-edit)
  (package-install 'sudo-edit))
(unless (package-installed-p 'counsel)
  (package-install 'counsel))
(unless (package-installed-p 'drag-stuff)
  (package-install 'drag-stuff))

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      indent-line-function 'insert-tab
      global-auto-revert-non-file-buffers t
      ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      counsel-find-file-at-point t
      completion-ignore-case t)

(define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

;; Enable completion by narrowing
;(use-package vertico
;  :ensure t
;  :custom
;  (vertico-cycle t)
;  (read-buffer-completion-ignore-case t)
;  (read-file-name-completion-ignore-case t)
;  (completion-styles '(basic substring partial-completion flex))
;  :init
;  (vertico-mode))

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

(global-auto-revert-mode)
(global-evil-mc-mode 1)
(multiple-cursors-mode)
(which-key-mode)
(counsel-mode)
(ido-mode)
(ivy-mode)
(editorconfig-mode)
(drag-stuff-global-mode t)
(global-display-line-numbers-mode nil)

(provide 'pache-misc)
;;; pache-misc.el ends here

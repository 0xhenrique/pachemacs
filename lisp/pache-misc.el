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
(unless (package-installed-p 'vertico)
  (package-install 'vertico))
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
(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

(setq-default
 indent-tabs-mode nil
 tab-width 4)

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      indent-line-function 'insert-tab
      global-auto-revert-non-file-buffers t
      ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      treemacs-indentation 2
      treemacs-indentation-string " "
      treemacs-hide-dot-git-directory t
      treemacs-move-forward-on-expand nil
      treemacs-position 'right
      treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
      treemacs-show-hidden-files t
      completion-ignore-case t)

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

;; Enable completion by narrowing
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Improve directory navigation
;(with-eval-after-load 'vertico
;  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
;  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
;  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))

;(use-package corfu
;  :ensure t
;  :init
;  (global-corfu-mode 1)
;  (corfu-popupinfo-mode 1)
;  :custom
;  (corfu-auto t)
;  ;; You may want to play with delay/prefix/styles to suit your preferences.
;  (corfu-auto-delay 0)
;  (corfu-auto-prefix 1)
;  (completion-styles '(basic)))

;; Enable autocompletion by default in programming buffers
;(add-hook 'prog-mode-hook #'corfu-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

(provide 'pache-misc)
;;; pache-misc.el ends here

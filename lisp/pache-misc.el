;;; pache-misc.el --- Everything that doesn't fit the other modules -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

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
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  (completion-styles '(basic)))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

(unless (package-installed-p 'move-text)
  (package-install 'move-text))
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; EditorConfig support
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
  (package-install 'eat))

;; Close the terminal buffer when the shell terminates.
(setq eat-kill-buffer-on-exit t)

;; Enable mouse-support.
(setq eat-enable-mouse t)

(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)

;; Redo command
(evil-set-undo-system 'undo-redo)

(use-package tab-bar
  :custom
  (tab-bar-format '(;tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                    tab-bar-format-align-right ; Optional: Remove to align left.
                    tab-bar-format-global))
  :config
  (tab-bar-mode 1))

(provide 'pache-misc)
;;; pache-misc.el ends here

;;; pache-misc.el --- Everything that doesn't fit the other modules -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(unless (package-installed-p 'ivy)
  (package-install 'ivy))

(unless (package-installed-p 'sudo-edit)
  (package-install 'sudo-edit))

;; Auto update files and dired
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; Emacs with super powers
(unless (package-installed-p 'ivy)
  (package-install 'ivy))
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(rainbow-delimiters-mode 1)

(unless (package-installed-p 'counsel)
  (package-install 'counsel))
(counsel-mode 1)

(ido-mode t)

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
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 1)
  (corfu-auto-prefix 1)
  (completion-styles '(basic)))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;;; EditorConfig support
(unless (package-installed-p 'editorconfig)
  (package-install 'editorconfig))

;; Enable EditorConfig
(editorconfig-mode t)

;;; Create a new shell with a new name
(defun pache/create-shell ()
    "Create a shell with a given name."
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;; Which-key displays available keybindings in popup
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)

;;; Org settings
;; Install Geiser to work with Scheme in Org files
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
;; Geiser-guile for Scheme
(unless (package-installed-p 'geiser-guile)
  (package-install 'geiser-guile))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

;; Helm
(unless (package-installed-p 'helm)
  (package-install 'helm))

(provide 'pache-misc)
;;; pache-misc.el ends here

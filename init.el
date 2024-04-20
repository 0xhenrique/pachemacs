;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Get some nice themes
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))

;; Load a custom theme
(load-theme 'gruvbox-dark-soft t)

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Comfy")

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
(vertico-mode t)

;; Improve directory navigation
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Enable line numbering by default
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;; Make eldoc work the way I want
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-message-function #'eldoc-minibuffer-message)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

;; Enable autocompletion by default in programming buffers
(add-hook 'prog-mode-hook #'corfu-mode)

;;; Git client
;;(unless (package-installed-p 'magit)
;;  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
;;(global-set-key (kbd "C-c g") #'magit-status)

;;; Indication of local VCS changes
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))

;; Enable `diff-hl' support by default in programming buffers
;;(add-hook 'prog-mode-hook #'diff-hl-mode)

;;; Go Support
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; Lua Support
(unless (package-installed-p 'lua-mode)
  (package-install 'lua-mode))

;;; PHP Support
(unless (package-installed-p 'php-mode)
  (package-install 'php-mode))

;;; Rust Support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))

;;; Additional Lisp support
(unless (package-installed-p 'sly)
  (package-install 'sly))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Outline-based notes management and organizer

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

;;; Vim Emulation
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Vim emulation
(evil-mode t)

;; Enable Vim emulation in programming buffers
(add-hook 'prog-mode-hook #'evil-local-mode)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Custom dashboard
(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))

(use-package dashboard
  :preface
  (defun create-scratch-buffer ()
     "Create a scratch"
     (interactive)
     (switch-to-buffer (get-buffer-create "*scratch*"))
     (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  (setq dashboard-banner-logo-title "H M - 2 0 3 0")
  (setq dashboard-banner-logo-title "\n")
  (setq dashboard-startup-banner "~/.emacs.d/marivector.png")
  (setq dashboard-center-content t)
  (setq dashboard-footer-messages '("\"I... I'm confused again.\""))
  (setq dashboard-footer-icon "")
  (setq dashboard-set-navigator t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))

  (setq dashboard-navigator-buttons
        `(;; line1
          ((,nil
            "Open scratch buffer"
            "Switch to the scratch buffer"
            (lambda (&rest _) (create-scratch-buffer))
            'default)
           (nil
            "Open init.el"
            "Open init.el for fast configuration"
            (lambda (&rest _) (find-file "~/.emacs.d/init.el"))
            'default)))))
;; With Emacs as daemon mode, when running `emacsclient`, open *dashboard* instead of *scratch*.
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

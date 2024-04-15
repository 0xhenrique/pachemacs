(use-package spacemacs-theme :defer t)
(use-package doom-themes :defer t)
(use-package modus-themes :defer t)

;; Source block expansion
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(use-package org
    :config
    ;(setq org-startup-with-inline-images t)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (add-hook 'org-mode-hook 'prettify-symbols-mode)
    ;(add-hook 'org-mode-hook 'org-toggle-pretty-entities)
    (add-hook 'org-mode-hook
              '(lambda ()
                 (visual-line-mode 1))))

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)

;; Custom header sizes.
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; Don't split src window.
(setq org-src-window-setup 'current-window)

(use-package org-indent
    :straight nil)

(setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAIT(w)"
           "|"                 ; Separates "active" and "inactive" states.
           "DONE(d)"
           "CANCELLED(c)" )))

(use-package htmlize)

;; Better org-bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; Automatically create a table of contents.
(use-package toc-org
  :after (org-mode markdown-mode)
  :hook
  (org-mode-hook . toc-org-mode)
  (markdown-mode-hook . toc-org-mode))

;; Evil bindings for Org mode.
(use-package evil-org
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Make invisible parts of Org elements appear visible.
(use-package org-appear
  :config
  (add-hook 'org-mode-hook 'org-appear-mode))

;; Automatically toggle Org mode LaTeX fragment previews as the cursor enters and exits them.
(use-package org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package org-modern
  :disabled
  ;:ensure nil
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star nil)
  (org-modern-hide-stars nil)
  (org-modern-variable-pitch nil)
  (org-modern-label-border 0.1))

(use-package org-modern-indent
  ;:after org-modern
  :straight (org-modern-indent :host github :repo "jdtsmith/org-modern-indent")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; Properly align tables containing variable-pitch font, CJK characters and images.
(use-package valign
  :hook
  (org-mode . valign-mode)
  (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar 1))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Notes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   ;; "d" is the letter you'll press to choose the template.
   ;; "default" is the full name of the template.
   ;; plain is the type of text being inserted.
   ;; "%?" is the text that will be inserted.
   ;; unnarrowed t ensures that the full file will be displayed when captured.
    '(("d" "default" plain "%?"
       :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
       :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
       :unnarrowed t)))
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n o" . org-id-get-create)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-setup)
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package vterm
  :defer t
  :straight t
  :config
  (setq vterm-shell "/usr/bin/zsh"))

(use-package multi-vterm
  :after vterm
  :defer t
  :straight (:build t))

(setq load-prefer-newer t)

; (setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-backup-file nil)
(setq auto-save-default nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(global-set-key (kbd "<f9>") 'display-line-numbers-mode)

(show-paren-mode 1)

;(setq next-line-add-newlines t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-fringe-mode 7)
;(setq-default fringes-outside-margins nil)
;(setq-default indicate-buffer-boundaries nil)
;(setq-default indicate-empty-lines nil)
;(setq-default overflow-newline-into-fringe t)

(setq x-select-enable-clipboard t)

(setq inhibit-startup-message t)

(setq scroll-conservatively 1)

(setq ring-bell-function 'ignore)

(global-prettify-symbols-mode t)

(add-hook 'org-mode-hook (lambda ()
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (prettify-symbols-mode)))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ;; spaces instead of tabs
(setq-default standard-indent 4)
(setq c-basic-offset tab-width)
(setq-default electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'nil)

;; Keeps code always indented.
;;(use-package aggressive-indent
;;  :diminish aggressive-indent-mode
;;  :config
;;  (add-hook 'prog-mode-hook #'aggressive-indent-global-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode . (lambda ()
                      (highlight-indent-guides-mode)
                      (highlight-indent-guides-auto-set-faces))))
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top))

(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(add-hook 'after-init-hook 'global-hl-line-mode)

(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")
                            ))
(electric-pair-mode t)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(use-package dimmer
  :disabled
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb
        dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))

(defun config-visit()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtine"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(use-package minions
  :config (minions-mode 1)
  (setq minions-mode-line-lighter "☰"))

(use-package exec-path-from-shell
  :init)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package nerd-icons
  :custom
  (nerd-icons-scale-factor 1.0)
  (nerd-icons-default-adjust 0.0))

(use-package nerd-icons-completion
  :straight
  (nerd-icons-completion :type git :host github
                         :repo "rainstormstudio/nerd-icons-completion")
  :demand t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :straight (nerd-icons-dired :type git :host github
                              :repo "rainstormstudio/nerd-icons-dired")
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :straight (treemacs-nerd-icons :type git :host github
                                 :repo "rainstormstudio/treemacs-nerd-icons")
  :config
  (with-eval-after-load 'treemacs
    (treemacs-load-theme "nerd-icons")))

(use-package which-key
  :init (which-key-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree))

;; Extra bindings for Evil
(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

;; Commenting lines
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Multiple cursors in evil-mode
(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))

(use-package popup-kill-ring
  :bind ("M-y" . popup-kill-ring))

(use-package beacon
  :init
  (beacon-mode 1))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
    '(("FIXME" error bold)
      ("TODO" org-todo)
      ("DONE" org-done)
      ("NOTE" bold))))

(use-package flyspell)

(use-package guess-language
  :config
  (setq guess-language-languages '(en pt))
  (setq guess-language-min-paragraph-length 10)
  :hook
  (text-mode . guess-language-mode))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;(add-hook
; 'prog-mode-hook
; (lambda ()
;   (ispell-change-dictionary "english")
;   (flyspell-prog-mode)))

(use-package avy
   :bind
   ("M-s" . avy-goto-char))

(defun def/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent folder otherwise delete word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . def/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  (vertico-scroll-margin 1)
  ;;(vertico-resize t)
  :init
  (vertico-mode))

;; Persistent history.
(use-package savehist
  :straight nil
  :init
  (setq history-length 25)
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

; C-s: ctrlf-forward-default (originally isearch-forward)
; C-r: ctrlf-backward-default (originally isearch-backward)
; C-M-s: ctrlf-forward-alternate (originally isearch-forward-regexp)
; C-M-r: ctrlf-backward-alternate (originally isearch-backward-regexp)
; M-s _: ctrlf-forward-symbol (originally isearch-forward-symbol)
; M-s .: ctrlf-forward-symbol-at-point (originally isearch-forward-symbol-at-point)
(use-package ctrlf
  :init (ctrlf-mode +1))

(use-package switch-window
   :config
   (setq switch-window-input-style 'minibuffer)
   (setq switch-window-increase 4)
   (setq switch-window-threshold 2)
   (setq switch-window-shortcut-style 'qwerty)
   (setq switch-window-qwerty-shortcuts
         '("a" "s" "d" "f" "j" "k" "l"))
   (setq switch-window-minibuffer-shortcut ?z)
   :bind
   ([remap other-window] . switch-window))

(use-package rainbow-mode
  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :init
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package doom-modeline
  :after (nerd-icons)
  :config
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-height 30)
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-unicode-fallback t)
  :hook (after-init . doom-modeline-mode))

(use-package awesome-tab
  :init
  (setq awesome-tab-height 110)
  (setq awesome-tab-show-tab-index t)
  :config
  (awesome-tab-mode t))

;; Alt+number to switch tabs.
(global-set-key (kbd "M-1") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-2") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-3") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-4") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-5") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-6") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-7") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-8") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-9") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "M-0") 'awesome-tab-select-visible-tab)

(use-package treemacs
  :config
  (setq treemacs-width 30)
  :bind (:map global-map
              ("C-x t t" . treemacs)
              ("C-x t 1" . treemacs-select-window)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  ;:disabled
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package page-break-lines
  :requires dashboard
  :init
    (global-page-break-lines-mode))

(use-package dashboard
  :preface
  (defun create-scratch-buffer ()
     "Create a scratch buffer"
     (interactive)
     (switch-to-buffer (get-buffer-create "*scratch*"))
     (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  ;(setq dashboard-banner-logo-title "Y U K I M A C S")
  (setq dashboard-banner-logo-title "\n")
  (setq dashboard-startup-banner "~/.emacs.d/img/lain-1.png")
  (setq dashboard-center-content t)
  ;(setq dashboard-init-info (format "Loaded in %s" (emacs-init-time)))
  ;(setq dashboard-set-footer nil)
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
            "Yukimacs on GitHub"
            "Open yukimacs' GitHub on your browser"
            (lambda (&rest _) (browse-url "https://github.com/pprobst/yukimacs"))
            'default)
           (,nil
            "Open scratch buffer"
            "Switch to the scratch buffer"
            (lambda (&rest _) (create-scratch-buffer))
            'default)
           (nil
            "Open config.org"
            "Open yukimacs' config file for easy editing"
            (lambda (&rest _) (find-file "~/.emacs.d/config.org"))
            'default)))))
;; With Emacs as daemon mode, when running `emacsclient`, open *dashboard* instead of *scratch*.
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;(use-package gptel
;  :config
;  (let* ((open-ai-auth (car (auth-source-search :host "OpenAI"))))
;    (setq gptel-api-key (plist-get open-ai-auth :api_key))))

(use-package projectile
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  ;(setq projectile-track-known-projects-automatically nil)
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t)
  (setq projectile-dynamic-mode-line nil))

(use-package yasnippet
  :config
    ;;(use-package yasnippet-snippets)
    ;;(use-package auto-yasnippet)
  (yas-reload-all)
  (yas-global-mode))

;; Collection of snippets from Doom Emacs.
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(global-set-key (kbd "C-c y") 'yas-insert-snippet)

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package eldoc
  :hook (after-init . global-eldoc-mode))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-log-io t)
  (read-process-output-max (* 1024 1024)) 
  (lsp-idle-delay 0.5)
  (lsp-prefer-flymake nil))

;; Enhance UI
(use-package lsp-ui
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
      ([remap xref-find-references] . lsp-ui-peek-find-references)
      ("C-c u" . lsp-ui-imenu)
      ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
      ("M-n" . forward-paragraph)
      ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-delay 3.0)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-delay 0.5))

;(use-package dap-mode
;  :after lsp-mode
;  :config
;  (dap-mode t)
;  (dap-ui-mode t))

(use-package lsp-treemacs
  :after (lsp-mode)
  :commands lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1))

(setq lsp-lens-enable nil)
;(setq lsp-enable-file-watchers nil)

(use-package company
   :after lsp-mode
   :bind
   (:map company-active-map
         ("C-n". company-select-next)
         ("C-p". company-select-previous)
         ("M-<". company-select-first)
         ("M->". company-select-last)
         ("<tab>" . company-complete-selection))
   (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
   :config
   (setq company-dabbrev-other-buffers t
         company-dabbrev-code-other-buffers t)
   :custom
   (company-minimum-prefix-length 1)
   (company-idle-delay 0.5)
   :hook ((text-mode . company-mode)
          (prog-mode . company-mode)
          (org-mode . company-mode)
          (company-mode . yas-minor-mode)
          (lsp-mode . company-mode)))

 (use-package company-box
   :hook (company-mode . company-box-mode)
   :config
   (setq company-box-max-candidates 50))

 (use-package company-prescient
   :after (selectrum company)
   :config
   (company-prescient-mode 1)
   (prescient-persist-mode))

(add-hook 'after-init-hook 'global-company-mode)

(use-package ccls
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq ccls-sem-highlight-method 'overlay)
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp) (yas-minor-mode))))

(setq-default c-basic-offset 4)

(use-package cmake-mode)

(use-package lsp-pyright
:hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
:init (when (executable-find "python3")
        (setq lsp-pyright-python-executable-cmd "python3")))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl") ;; install SBCL from your repos
  (setq slime-contribs '(slime-fancy)))

;; Better help buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package haskell-mode
  :config
  (use-package lsp-haskell)
  (require 'lsp)
  (require 'lsp-haskell)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'yas-minor-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  (setq haskell-stylish-on-save t))

;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/#code-completion-and-snippets

(use-package rustic
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-flycheck-setup-mode-line-p nil)
  :hook ((rustic-mode . (lambda ()
                          (lsp-ui-doc-mode)
                          (company-mode)
                          (yas-minor-mode))))
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rust-indent-method-chain t)
  (setq rustic-format-on-save t))

(use-package flycheck-rust)

(use-package go-mode
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package ron-mode
  :mode (("\\.ron\\'" . ron-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package yaml-mode
  :commands yaml-mode)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package auctex
  :hook
  (TeX-mode . TeX-PDF-mode)
  (TeX-mode . company-mode)
  (LaTeX-mode . (lambda ()
                  (push (list 'output-pdf "Zathura")
                            TeX-view-program-selection)))
  :init
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-open-quote  "<<")
  (setq TeX-close-quote ">>")
  (setq TeX-electric-sub-and-superscript t)
  (setq font-latex-fontify-script nil)
  (setq TeX-show-compilation nil)
  (setq reftex-label-alist '(AMSTeX)))

(use-package company-auctex
  :init
  (company-auctex-init))

(use-package company-reftex
  :init
  (add-to-list 'company-backends 'company-reftex-citations)
  (add-to-list 'company-backends 'company-reftex-labels))

(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(use-package treemacs-magit
  :after treemacs magit)

(use-package ghub
  :demand t
  :after magit)

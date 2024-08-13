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

;; Get some nice themes and icons
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'catppuccin-theme)
  (package-install 'catppuccin-theme))

(use-package all-the-icons
  :if (display-graphic-p))

;; Load a custom theme
(load-theme 'gruvbox-dark-hard t)

;; Set default font face
(set-face-attribute 'default nil :font "Iosevka Comfy")

;; Some basic visual tweak
(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
(setq scroll-conservatively 100)
(setq display-time-default-load-average nil)

;;; EXWM Setup
(require 'exwm-randr)
;; (require 'exwm-systemtray)
(exwm-randr-mode t)
(start-process-shell-command
 "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-2 --auto")
(exwm-systemtray-mode t)

;; (setq exwm-manage-configurations
;;       '(((string-equal exwm-class-name "Dialog")
;;          floating t)
;;         ((string-equal exwm-instance-name "Firefox: Save Image")
;;          floating t)))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf: %s" exwm-title)))
    ("Transmission-gtk" (exwm-workspace-rename-buffer (format "Transmission: %s" exwm-title)))))
  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

(defvar polybar-process nil)
(defun kill-polybar ()
  (interactive)
  (when polybar-process
    (ignore-errors
      (kill-process polybar-process)))
  (setq polybar-process nil))
(defun start-polybar ()
  (interactive)
  (kill-polybar)
  (setq polybar-process (start-process-shell-command "polybar" nil "polybar")))

;; (start-process-shell-command
;;  "polybar" nil "polybar")

(defun set-wallpaper()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale ~/Pictures/papes/gun-waifu.png"))

;; (set-frame-parameter (selected-frame) 'alpha '(93 . 93))
;; (add-to-list 'default-frame-alist 'alpha '(93 . 93))

(ido-mode 1)
(display-time-mode t)
(unless (package-installed-p 'exwm)
  (package-install 'exwm))
(setq exwm-workspace-number 4)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
	;; Move between windows
	([s-left] . windmove-left)
	([s-right] . windmove-right)
	([s-up] . windmove-up)
	([s-down] . windmove-down)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))
(setq exwm-workspace-minibuffer-position 'bottom)
(set-wallpaper)
(exwm-enable)

;; Desktop keys
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ +2%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -2%")))

;; (exwm-input-set-key (kbd "s-<right>") 'windmove-right)
;; (exwm-input-set-key (kbd "s-<left>") 'windmove-left)
;; (exwm-input-set-key (kbd "s-<down>") 'windmove-down)
;; (exwm-input-set-key (kbd "s-<up>") 'windmove-up)
;; (exwm-input-set-key (kbd "S-s-SPC") 'exwm-floating-toggle-floating)

;;; Emacs evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;;; Completion framework
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable completion by narrowing
;(vertico-mode t)
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

;; Enable line numbering by default
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Automatically pair parentheses
(electric-pair-mode t)

;;; LSP Support
(unless (package-installed-p 'eglot)
  (package-install 'eglot))

(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

;;(unless (package-installed-p 'lsp-ui)
;;  (package-install 'lsp-ui))
;;
;;; Vue Support
(unless (package-installed-p 'vue-mode)
  (package-install 'vue-mode))
;;(add-hook 'vue-mode-hook #'lsp)

;;; Flycheck Support
(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; NVM for Node development
(unless (package-installed-p 'nvm)
  (package-install 'nvm))

;;; Music Player inside Emacs!
(unless (package-installed-p 'emms)
  (package-install 'emms))

;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  ;; Add your programming modes here to automatically start Eglot,
  ;; assuming you have the respective LSP server installed.
  :hook ((web-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :config
  ;; You can configure additional LSP servers by modifying
  ;; `eglot-server-programs'. The following tells eglot to use TypeScript
  ;; language server when working in `web-mode'.
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  )

;; Enable LSP support by default in programming buffers
;;(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;; Make eldoc work the way I want
;;(setq eldoc-echo-area-use-multiline-p nil)
;;(setq eldoc-message-function #'eldoc-minibuffer-message)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))

;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;;:bind
  ;;(("M-b" . completion-at-point))
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

;;; Yasnippets FTW
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
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

(unless (package-installed-p 'consult)
  (package-install 'consult))

(unless (package-installed-p 'move-text)
  (package-install 'move-text))
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;;; Setting some custom keybinds
;; Define `SPC` as a prefix key in normal mode
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

;; Redo command
(evil-set-undo-system 'undo-redo)

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

;;; Custom dashboard
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

  (setq
   dashboard-startup-banner "~/.emacs.d/ascii/3.txt"
   dashboard-banner-logo-title "H M - 2 0 3 0"
   dashboard-banner-logo-title "\n"
   dashboard-center-content t
   dashboard-vertically-center-content t
   dashboard-footer-messages '("
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⣶⣶⣶⣶⣤⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣶⣶⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢈⣹⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠊⣿⣿⣟⡿⣿⣿⣿⣿⡿⡁⣿⣿⣿⠀⠀⠀⠀⣀⣤⡾⣦⣄⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⣀⣴⣾⠋⠉⣻⡷⢦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠃⢽⣿⣿⠿⢻⠏⠑⢀⣿⣿⣿⣀⣠⡶⠋⢹⣷⣄⢠⣿⣿⡶⣤⡀⠀⠀⠀
⠀⠀⣀⣴⣾⢫⣿⡿⠀⣾⣿⠇⠀⣼⡟⠛⠛⣻⣿⣧⣀⣴⣶⣾⣿⣿⣷⣶⣄⣰⡟⣻⣿⡟⠉⣿⣷⡀⠘⣿⣿⠀⠙⢿⣇⢼⣿⣷⢦⣄
⣴⡞⢹⣿⡿⣸⠟⠀⠀⣿⠏⠀⢸⣿⡟⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⣿⣿⡇⠀⠻⣿⡇⠀⠈⢻⠀⠀⠀⠛⠌⠻⣿⡠⣿
⣿⡏⡿⠏⠀⠁⠀⠀⠀⠃⠀⠀⠸⡿⠀⠀⢸⣿⣻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣙⣿⠁⠀⠀⠹⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠃⠙
⠋⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠁⠀⠀⢘⠃⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡆⠸⠀⠀⠀⠀⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⣿⣿⠛⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣿⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣰⡀⢀⡀⣻⣄⡀⢀⣠⣴⣿⣿⣿⠃⢠⣿⣿⣿⣿⣿⣿⣿⣿⡆⠹⣿⣿⣿⣶⣄⡀⢀⣰⣏⣀⣀⣠⣇⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠙⣿⣿⣿⣿⣿⣿⣿⣿⡿⠟⠋⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⣿⣇⠀⠀⠙⠻⢿⣿⣿⣿⣿⣿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠈⠁⠈⠉⠛⠋⠁⠀⠀⠀⠀⢰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣇⠀⠀⠀⠀⠈⠙⠛⠛⠉⠉⠛⠀⠀⠀⠀⠀⠀⠀⠀
")
   dashboard-footer-icon ""
   ;;dashboard-set-navigator t
   ;;dashboard-set-heading-icons t
   dashboard-items '((recents  . 10)
		     (bookmarks . 5)
		     (projects . 5))
   )

  ;;(setq dashboard-icon-type 'all-the-icons)
  ;;(setq dashboard-display-icons-p t)
  ;;(setq dashboard-set-heading-icons t)
  ;;(setq dashboard-set-file-icons t)

  (setq dashboard-startupify-list '(
				    ;;dashboard-insert-newline
				    ;;dashboard-insert-banner-title
				    ;;dashboard-insert-footer
				    ;;dashboard-insert-newline
				    ;;dashboard-insert-navigator
				    ;;dashboard-insert-newline
				    dashboard-insert-items
				    dashboard-insert-banner
				    dashboard-insert-init-info))
				    ;;dashboard-insert-newline))

  (setq dashboard-navigation-cycle t)
  (setq dashboard-heading-shorcut-format " [%s]")
  (setq dashboard-item-shortcuts '((recents   . "r")
				   (marks . "m")
				   (projects  . "p"))))

;;  (setq dashboard-navigator-buttons
;;        `(
;;          ((,"<C-p C-0>"
;;            "Open scratch buffer"
;;            "Switch to the scratch buffer"
;;            (lambda (&rest _) (create-scratch-buffer))
;;            'default)
;;           (nil
;;            "Open init.el"
;;            "Open init.el for fast configuration"
;;            (lambda (&rest _) (find-file "~/.emacs.d/init.el"))
;;            'default)))))
;; With Emacs as daemon mode, when running `emacsclient`, open *dashboard* instead of *scratch*.
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; TypeScript, JS, and JSX/TSX support.
(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
	      ("C-c C-r" . 'rust-run)
	      ("C-c C-c" . 'rust-compile)
	      ("C-c C-f" . 'rust-format-buffer)
	      ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))

(use-package yaml-mode
  :ensure t)

;; Note that `php-mode' assumes php code is separate from HTML.
;; If you prefer working with PHP and HTML in a single file you
;; may prefer `web-mode'.
(use-package php-mode
  :ensure t)

;; Some custom options
(setq
 auto-save-default nil
 scroll-step 1
 sentence-end-double-space nil
 backup-directory-alist `(("." . "~/.saves"))
 inhibit-startup-echo-area-message "I'm confused again"
 )

;;(setq-default inhibit-splash-screen
;;	      tab-width 4
;;	      indent-tabs-mode nil)

;;; IRC stuff
;; Set nickname & real-name as constant variables
(setq
 erc-nick "HenriqMarq"     ; Our IRC nick
 erc-user-full-name "Henrique Marques") ; Our /whois name

;; Connect to server
(defun some-serv ()
  (interactive)
  (erc :server "colonq.computer"
       :port   "26697"))

;;; Some other tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* (* 1024 1024) 3)) ;; 3mb
(run-with-idle-timer 2 t (lambda () (garbage-collect)))
(setq lsp-log-io nil) ; if set to true can cause a performance hit
(setq lsp-idle-delay 1)
(setq lsp-auto-guess-root t)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-eldoc-hook nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-semantic-tokens-enable nil)
(setq lsp-enable-folding nil)
(setq lsp-enable-imenu nil)
(setq lsp-enable-snippet nil)
(setq read-process-output-max (* (* 1024 1024) 3)) ;; 3MB
(setq lsp-idle-delay 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(use-package i3bar
  :ensure t
  :config
  (i3bar-mode 1))

(use-package tab-bar
  :custom
  (tab-bar-format '(;tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
                    tab-bar-format-align-right ; Optional: Remove to align left.
                    tab-bar-format-global))
  :config
  (tab-bar-mode 1))

(provide 'init)
;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)

;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Code:
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ido-mode t)
(set-face-attribute 'default nil :font "Iosevka" :height 120)
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(setq scroll-conservatively 100
      resize-mini-windows 'grow-only
      display-time-default-load-average nil)

;; Performance
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

(require 'package)
(require 'uniquify)
(electric-pair-mode t)
(show-paren-mode 1)
(save-place-mode t)
(setq-default indent-tabs-mode nil)
(setq global-auto-revert-non-file-buffers t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)
(global-display-line-numbers-mode t)

;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Set the default theme
(load-theme 'anti-zenburn t)

;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (ef-themes-select 'ef-dark))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))

(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix)))

(use-package clojure-mode
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(evil-set-undo-system 'undo-redo)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package vterm
  :ensure t)

(use-package helm
  :ensure t
  :init
  (helm-mode))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x p w") 'helm-do-grep-ag)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x /") 'helm-occur)
(global-set-key (kbd "C-x g c") 'comment-region)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(unless (package-installed-p 'sudo-edit)
  (package-install 'sudo-edit))

; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(setq confirm-kill-emacs #'yes-or-no-p
      window-resize-pixelwise t
      frame-resize-pixelwise t
      auto-save-default nil
      scroll-step 1
      sentence-end-double-space nil
      backup-directory-alist `(("." . "~/.saves"))
      inhibit-startup-echo-area-message "The shrine isn't a good place for using magic.")

;; Set keyboard layout switch (US and ABNT2)
(start-process-shell-command
 "setxkbmap" nil "setxkbmap -layout 'us,br' -option 'grp:alt_shift_toggle'")

;; YouTube video download
(defun download-yt ()
  "Download a YouTube video URL interactively to ~/Videos."
  (interactive)
  (let* ((url (read-string "Enter YouTube URL: "))
         (quality (completing-read "Choose video quality: " '("Audio" "144p" "Best Quality") nil t))
         (videos-dir (expand-file-name "~/Videos/"))
         (command "")
         (file-name-template "%(title)s"))
    (cond
     ((equal quality "144p")
      (setq command (format "yt-dlp -f 'bestvideo[height<=144]+bestaudio' -o '%s%s' %s"
                           videos-dir file-name-template url)))
     ((equal quality "Best Quality")
      (setq command (format "yt-dlp -f 'bestvideo+bestaudio' -o '%s%s' %s"
                           videos-dir file-name-template url))))
    (start-process-shell-command "yt-dlp" "*yt-dlp*" command)
    (message "Downloading video: %s at %s quality" url quality)))

;; YouTube music download
(defun download-yt-audio ()
  "Download the audio from a YouTube video URL to a user-selected directory."
  (interactive)
  (let* ((url (read-string "Enter YouTube URL: "))
         (music-dir (read-directory-name "Choose download directory: " "~/Music/"))
         (command "")
         (file-name-template "%(title)s"))
    (setq command (format "yt-dlp -x --embed-metadata --audio-quality 0 --format bestaudio -o '%s%s.%%(ext)s' %s"
                          (file-name-as-directory music-dir) file-name-template url))
    (start-process-shell-command "yt-dlp" "*yt-dlp*" command)
    (message "Downloading audio: %s to %s" url music-dir)))

;; Kensington Orbit scroll utility - change the '11' to the actual ID from 'xinput list' command
(start-process-shell-command
 "xinput" nil "xinput set-prop 11 'libinput Middle Emulation Enabled' 1")
(start-process-shell-command
 "xinput" nil "xinput set-prop 11 'libinput Scroll Method Enabled' 0 0 1")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac" "f1c8202c772d1de83eda4765fe21429a528a4fb350a28394d3705fe9678ed1f9" "54a07e4250791390837b3b30289c49b4972cdf350fb12e6430715fc97087caf4" "deb645f30fd25191b6e8d0f397cc1dd172a352f22094747be2ff527394cc9f57" default))
 '(package-selected-packages
   '(acme-theme anti-zenburn-theme plan9-theme clojure-mode helm desktop-environment which-key vterm vertico vc-use-package sudo-edit rainbow-delimiters paredit marginalia magit helpful evil-collection ef-themes denote counsel corfu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

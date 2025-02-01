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

;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-c v") 'ivy-push-view)
;; (global-set-key (kbd "C-c V") 'ivy-pop-view)
;; (global-set-key (kbd "C-c c") 'counsel-compile)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c L") 'counsel-git-log)
;; (global-set-key (kbd "C-c k") 'counsel-rg)
;; (global-set-key (kbd "C-c m") 'counsel-linux-app)
;; (global-set-key (kbd "C-c n") 'counsel-fzf)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-c J") 'counsel-file-jump)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (global-set-key (kbd "C-c w") 'counsel-wmctrl)

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))
(rainbow-delimiters-mode 1)

(unless (package-installed-p 'counsel)
  (package-install 'counsel))
(counsel-mode 1)

(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-d") 'counsel-rg)

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

;; Customise dired
;;(unless (package-installed-p 'openwith)
;;  (package-install 'openwith))
;;(when (require 'openwith nil 'noerror)
;;  (setq openwith-associations
;;	(list
;;	 (list (openwith-make-extension-regexp
;;		'("mpg" "mpeg" "mp3" "mp4"
;;		  "avi" "wmv" "wav" "mov" "flv"
;;		  "ogm" "ogg" "mkv"))
;;	       "mpv"
;;	       '(file))
;;	 (list (openwith-make-extension-regexp
;;		'("xbm" "pbm" "pgm" "ppm" "pnm"
;;		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;	       "feh"
;;	       '(file))
;;	 (list (openwith-make-extension-regexp
;;		'("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
;;	       "libreoffice"
;;	       '(file))
;;	 '("\\.lyx" "lyx" (file))
;;	 '("\\.chm" "kchmviewer" (file))
;;	 (list (openwith-make-extension-regexp
;;		'("pdf" "ps" "ps.gz" "dvi"))
;;	       "librewolf"
;;	       '(file))
;;	 ))
;;  (openwith-mode 1))

;;(setq dired-guess-shell-alist-user '(("\\.pdf\\'" "librewolf")
;;				     ("\\.flac\\'" "mpv")
;;				     ("\\.mp3\\'" "mpv")
;;				     ("\\.mp4\\'" "mpv")
;;				     ("\\.webm\\'" "mpv")
;;				     ("\\.jpg\\'" "feh")
;;				     ("\\.png\\'" "feh")
;;				     ("\\.webp\\'" "feh")))

;; Enable EditorConfig
(editorconfig-mode t)

;;; Create a new shell with a new name
(defun create-shell ()
    "Creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

;;; In-Emacs Terminal Emulation
(unless (package-installed-p 'eat)
  (package-install 'eat))

;; Vterm
(unless (package-installed-p 'vterm)
  (package-install 'vterm))
(unless (package-installed-p 'multi-vterm)
  (package-install 'multi-vterm))

;; Close the terminal buffer when the shell terminates.
(setq eat-kill-buffer-on-exit t)

;; Enable mouse-support
(setq eat-enable-mouse t)

;; Which-key displays available keybindings in popup
(unless (package-installed-p 'which-key)
  (package-install 'which-key))
(which-key-mode)

;; Redo command
(evil-set-undo-system 'undo-redo)

;;(use-package tab-bar
;;  :custom
;;  (tab-bar-format '(;tab-bar-format-tabs        ; Optional: Remove to _only_ display the bar.
;;                    tab-bar-format-align-right ; Optional: Remove to align left.
;;                    tab-bar-format-global))
;;  :config
;;  (tab-bar-mode 1))

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

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x p f") 'project-find-file)
(global-set-key (kbd "C-x p w") 'helm-do-grep-ag)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x /") 'helm-occur)
(global-set-key (kbd "s-d") 'helm-run-external-command)

(provide 'pache-misc)
;;; pache-misc.el ends here

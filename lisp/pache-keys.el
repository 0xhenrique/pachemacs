;;; pache-keys.el --- Custom Keybindings for Emacs with evil-mode -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Bind commands to sequences starting with `SPC` in normal mode
(define-key evil-normal-state-map (kbd "SPC s g") #'counsel-rg)
(define-key evil-normal-state-map (kbd "SPC s f") #'project-find-file)
(define-key evil-normal-state-map (kbd "SPC s o") #'helm-occur)
(define-key evil-normal-state-map (kbd "SPC g s") #'magit-status)
(define-key evil-normal-state-map (kbd "SPC g g") #'counsel-git-grep)
(define-key evil-normal-state-map (kbd "SPC b l") #'helm-buffers-list)

(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; Exploratory
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x p f") 'project-find-file)
(global-set-key (kbd "C-x p w") 'helm-do-grep-ag)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x /") 'helm-occur)
(global-set-key (kbd "s-d") 'helm-run-external-command)

;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;(global-set-key (kbd "C-c v") 'ivy-push-view)
;;(global-set-key (kbd "C-c V") 'ivy-pop-view)
;;(global-set-key (kbd "C-c c") 'counsel-compile)
;;(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "C-c L") 'counsel-git-log)
;;(global-set-key (kbd "C-c m") 'counsel-linux-app)
;;(global-set-key (kbd "C-c n") 'counsel-fzf)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-c J") 'counsel-file-jump)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(provide 'pache-keys)
;;; pache-keys.el ends here

;;; pache-keys.el --- Custom Keybindings for Emacs with evil-mode -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Disable some evil-mode keybindings
(define-key evil-normal-state-map (kbd "S-k") nil)
(global-set-key (kbd "S-k") nil)

;; Buffer
(define-key evil-normal-state-map (kbd "SPC b l") #'counsel-switch-buffer)

;; Evaluation
(define-key evil-normal-state-map (kbd "SPC e r") #'eval-region)
(define-key evil-normal-state-map (kbd "SPC e b") #'eval-buffer)
(define-key evil-normal-state-map (kbd "SPC e l") #'eval-last-sexp)

;; File
(define-key evil-normal-state-map (kbd "SPC f a p") #'ffap)
(define-key evil-normal-state-map (kbd "SPC f t") #'treemacs)

;; Git
(define-key evil-normal-state-map (kbd "SPC g s") #'magit-status)
(define-key evil-normal-state-map (kbd "SPC g g") #'counsel-git-grep)

;; Project
(define-key evil-normal-state-map (kbd "SPC p w") #'counsel-rg)

;; Search
(define-key evil-normal-state-map (kbd "SPC s g") #'counsel-rg)
(define-key evil-normal-state-map (kbd "SPC s f") #'project-find-file)
(define-key evil-normal-state-map (kbd "SPC s o") #'swiper-isearch)

;; Evil Multiple Cursors
(global-set-key (kbd "C->") 'evil-mc-make-and-goto-next-match)
(global-set-key (kbd "C-<") 'evil-mc-make-and-goto-prev-match)
(global-set-key (kbd "C-t") 'evil-mc-skip-and-goto-next-match)
(define-key evil-normal-state-map (kbd "SPC m q") 'evil-mc-undo-all-cursors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace                                                            ;;
;; <n> -> next                                                        ;;
;; <^> -> last                                                        ;;
;; <u> -> undo last                                                   ;;
;; <y> or <SPC> accept                                                ;;
(define-key evil-normal-state-map (kbd "SPC r s") #'query-replace)    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function keys
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)

;; Other
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x p f") 'project-find-file)
(global-set-key (kbd "C-x p w") 'counsel-rg)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x /") 'swiper-isearch)

;; Movement
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)
(global-set-key (kbd "M-<left>") 'drag-stuff-left)
(global-set-key (kbd "M-<right>") 'drag-stuff-right)

;; Improve directory navigation with Vertico
;(with-eval-after-load 'vertico
;  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
;  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
;  (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))

;; Unused
;(global-set-key (kbd "M-x") 'counsel-M-x)
;(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;(global-set-key (kbd "s-d") 'helm-run-external-command)
;(global-set-key (kbd "C-c v") 'ivy-push-view)
;(global-set-key (kbd "C-c V") 'ivy-pop-view)
;(global-set-key (kbd "C-c c") 'counsel-compile)
;(global-set-key (kbd "C-c g") 'counsel-git)
;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;(global-set-key (kbd "C-c L") 'counsel-git-log)
;(global-set-key (kbd "C-c m") 'counsel-linux-app)
;(global-set-key (kbd "C-c n") 'counsel-fzf)
;(global-set-key (kbd "C-x l") 'counsel-locate)
;(global-set-key (kbd "C-c J") 'counsel-file-jump)
;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(provide 'pache-keys)
;;; pache-keys.el ends here

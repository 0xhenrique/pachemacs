;;; pache-misc.el --- Everything that doesn't fit the other modules -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(dolist (pkg '(editorconfig
			   which-key
			   geiser
			   geiser-guile
			   magit
			   ivy
			   evil-mc
			   sudo-edit
			   sudo-edit
			   counsel
			   drag-stuff))
  (unless (package-installed-p pkg)
	(package-install pkg)))

(setq-default tab-width 4
			  standard-indent 4
			  electric-indent-inhibit t
			  indent-tabs-mode t)

(setq read-buffer-completion-ignore-case t
	  backward-delete-char-untabify-method 'nil
      read-file-name-completion-ignore-case t
      indent-line-function 'insert-tab
      global-auto-revert-non-file-buffers t
      ivy-count-format "(%d/%d) "
      ivy-use-virtual-buffers t
      counsel-find-file-at-point t
      completion-ignore-case t)

;; Guess major mode from file name
(setq-default major-mode
              (lambda ()
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (scheme . t)))

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

(provide 'pache-misc)
;;; pache-misc.el ends here

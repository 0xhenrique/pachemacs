;;; pache-ai.el --- AI stuff goes here -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(add-to-list 'load-path "~/.emacs.d/supermaven")
(require 'supermaven)
(add-hook 'prog-mode-hook 'supermaven-mode)

(setq supermaven-ignore-filetypes '("org" "txt"))
(setq supermaven-disable-inline-completion nil)
(setq supermaven-keymaps
      '((accept-suggestion . "TAB")
        (clear-suggestion . "C-]")
        (accept-word . "C-j")))
(setq supermaven-log-level 'debug)

;(unless (package-installed-p 'gptel)
;  (package-install 'gptel))
;(unless (package-installed-p 'copilot)
;  (package-install 'copilot))

;(require 'copilot)
;(add-hook 'prog-mode-hook 'copilot-mode)
;(copilot-mode t)
;(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(provide 'pache-ai)
;;; pache-ai.el ends here

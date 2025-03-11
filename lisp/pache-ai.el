;;; pache-ai.el --- AI stuff goes here -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(unless (package-installed-p 'gptel)
  (package-install 'gptel))

(unless (package-installed-p 'copilot)
  (package-install 'copilot))
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(copilot-mode t)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(provide 'pache-ai)
;;; pache-ai.el ends here

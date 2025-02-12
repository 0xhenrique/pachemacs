;;; pache-ai.el --- AI stuff goes here -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(unless (package-installed-p 'gptel)
  (package-install 'gptel))

(unless (package-installed-p 'copilot)
  (package-install 'copilot))

(provide 'pache-ai)
;;; pache-ai.el ends here

;;; pache-keys.el --- Custom Keybindings for EXWM and Emacs -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Firefox Search
(defun firefox-search-term (term)
  "Prompt for a search term and open Firefox to search for it."
  (interactive "sFirefox search term: ")
  (start-process-shell-command
   "firefox" nil (concat "firefox --search " (shell-quote-argument term))))

;; Librewolf Search
(defun librewolf-search-term (term)
  "Prompt for a search term and open Librewolf to search for it."
  (interactive "sLibrewolf search term: ")
  (start-process-shell-command
   "librewolf" nil (concat "librewolf --search " (shell-quote-argument term))))

(provide 'pache-keys)
;;; pache-keys.el ends here

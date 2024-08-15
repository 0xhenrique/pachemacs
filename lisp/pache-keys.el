;;; pache-keys.el --- Custom Keybindings for EXWM and Emacs -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Firefox Search
(defun firefox-search-term (term)
  "Prompt for a search term and open Firefox to search for it."
  (interactive "sEnter search term: ")
  (start-process-shell-command
   "firefox" nil (concat "firefox --search " (shell-quote-argument term))))

;; Librewolf Search
(defun librewolf-search-term (term)
  "Prompt for a search term and open Librewolf to search for it."
  (interactive "sEnter search term: ")
  (start-process-shell-command
   "librewolf" nil (concat "librewolf --search " (shell-quote-argument term))))

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ +2%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -2%")))
(exwm-input-set-key (kbd "s-q") (kill-this-buffer))
(exwm-input-set-key (kbd "s-f") 'firefox-search-term)
(exwm-input-set-key (kbd "s-l") 'firefox-search-term)

(provide 'pache-keys)
;;; pache-keys.el ends here

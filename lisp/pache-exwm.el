;;; pache-exwm.el --- EXWM Settings -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(require 'exwm-randr)
;; This is needed because I prefer to only use the main monitor
(start-process-shell-command
 "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-2 --auto")

;; Set keyboard layout switch (US and ABNT2)
(start-process-shell-command
 "setxkbmap" nil "setxkbmap -layout 'us,br' -option 'grp:alt_shift_toggle'")

;; Set wallpaper
(start-process-shell-command
 "feh" nil "feh --bg-scale ~/Pictures/papes/patchy/3.jpg")

;; Start picom
(start-process-shell-command
 "picom" nil "picom")

;; Firefox Search
(defun pache/firefox-search-term (term)
  "Prompt for a search TERM and open Firefox to search for it."
  (interactive "sFirefox search term: ")
  (start-process-shell-command
   "firefox" nil (concat "firefox --search " (shell-quote-argument term))))

;; Librewolf Search
(defun pache/librewolf-search-term (term)
  "Prompt for a search TERM and open Librewolf to search for it."
  (interactive "sLibrewolf search term: ")
  (start-process-shell-command
   "librewolf" nil (concat "librewolf --search " (shell-quote-argument term))))

(defun pache/exwm-update-title ()
  (pcase exwm-class-name
    ;; This is used to avoid the buffers being named like "librewolf<3>", so we use that window's title as buffer name
    ("LibreWolf" (exwm-workspace-rename-buffer (format "Librewolf: %s" exwm-title)))
    ("multi-vterm" (exwm-workspace-rename-buffer (format "Vterm: %s" exwm-title)))
    ("Transmission-gtk" (exwm-workspace-rename-buffer (format "Transmission: %s" exwm-title)))))

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'pache/exwm-update-title)
;; (add-hook 'exwm-init-hook (lambda () (dashboard-refresh-buffer)))
;; (add-hook 'after-init-hook
;; 	  (lambda ()
;; 	    (switch-to-buffer "*dashboard*")))

;;  If you want to set a wallpaper, uncomment the following function and set a correct path to feh
; (defun set-wallpaper()
;   (interactive)
;   (start-process-shell-command "feh" nil "feh --bg-scale ~/Pictures/Wallpapers/waifu.png"))

;; Maybe you'll want to set some transparency when using a wallpaper
; (set-frame-parameter (selected-frame) 'alpha '(93 . 93))
; (add-to-list 'default-frame-alist 'alpha '(93 . 93))

(display-time-mode t)
(unless (package-installed-p 'exwm)
  (package-install 'exwm))
(setq exwm-workspace-number 3)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; A Windows PC forced me to do this since I couldn't change the keybinds there lol
(defun pache/exwm-switch-workspace (direction)
  "Choose the DIRECTION."
  (interactive "sDirection: ")
  (let ((current-workspace exwm-workspace-current-index))
    (cond
     ((string= direction "left") (exwm-workspace-switch (1- current-workspace)))
     ((string= direction "right") (exwm-workspace-switch (1+ current-workspace))))))

;;helm-M-x-execute-command: s- must prefix a single character, not left
;;All interactive functions should have documentation
(exwm-input-set-key (kbd "C-s-<left>") (lambda () (interactive) (exwm-switch-workspace "left")))
(exwm-input-set-key (kbd "C-s-<right>") (lambda () (interactive) (exwm-switch-workspace "right")))

(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
	;; Move between windows
	([s-left] . windmove-left)
	([s-right] . windmove-right)
	([s-up] . windmove-up)
	([s-down] . windmove-down)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-d] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
		    (interactive)
		    (start-process "" nil "/usr/bin/slock")))))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))
;; (setq exwm-workspace-minibuffer-position 'top)
;; (set-wallpaper)
(exwm-enable)

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ +2%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -2%")))

(exwm-input-set-key (kbd "s-q") (kill-this-buffer))
(exwm-input-set-key (kbd "s-f") 'firefox-search-term)
(exwm-input-set-key (kbd "s-l") 'librewolf-search-term)
(exwm-input-set-key (kbd "s-SPC") 'ivy-switch-buffer)
(exwm-input-set-key (kbd "s-k") 'ivy-switch-buffer-kill)

;; YouTube Download - must have yt-dlp installed
(defun pache/download-yt ()
  "Download a YouTube video URL interactively to ~/Videos."
  (interactive)
  (let* ((url (read-string "Enter YouTube URL: "))
         (quality (completing-read "Choose video quality: " '("144p" "Best Quality") nil t))
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

(provide 'pache-exwm)
;;; pache-exwm.el ends here

;;; pache-exwm.el --- EXWM Settings -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(require 'exwm-randr)
;; This is needed because I prefer to only use the main monitor
(start-process-shell-command
 "xrandr" nil "xrandr --output eDP-1 --off --output HDMI-2 --auto")

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ;; This is used to avoid the buffers being named like "firefox<3>", so we use that window's title as buffer name
    ("firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))
    ("librewolf" (exwm-workspace-rename-buffer (format "Librewolf: %s" exwm-title)))
    ("Transmission-gtk" (exwm-workspace-rename-buffer (format "Transmission: %s" exwm-title)))))

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'efs/exwm-update-title)
(add-hook 'exwm-init-hook (lambda () (dashboard-refresh-buffer)))
(add-hook 'after-init-hook
	  (lambda ()
	    (switch-to-buffer "*dashboard*")))

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
(setq exwm-workspace-number 10)

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
        ([?\s-&] . (lambda (command)
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
(setq exwm-workspace-minibuffer-position 'bottom)
;; (set-wallpaper)
(exwm-enable)

;; Desktop keys
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ +2%")))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>")
                    (lambda () (interactive)
                      (start-process-shell-command "pactl" nil "pactl set-sink-volume @DEFAULT_SINK@ -2%")))

(use-package i3bar
  :ensure t
  :config
  (i3bar-mode 1))

(provide 'pache-exwm)
;;; pache-exwm.el ends here

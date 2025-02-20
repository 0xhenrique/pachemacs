;;; pache-utils.el --- General Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set keyboard layout switch (US and ABNT2)
(start-process-shell-command
 "setxkbmap" nil "setxkbmap -layout 'us,br' -option 'grp:alt_shift_toggle'")

;; YouTube video download
(defun download-yt ()
  "Download a YouTube video URL interactively to ~/Videos."
  (interactive)
  (let* ((url (read-string "Enter YouTube URL: "))
         (quality (completing-read "Choose video quality: " '("Audio" "144p" "Best Quality") nil t))
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

;; YouTube music download
(defun download-yt-audio ()
  "Download the audio from a YouTube video URL to a user-selected directory."
  (interactive)
  (let* ((url (read-string "Enter YouTube URL: "))
         (music-dir (read-directory-name "Choose download directory: " "~/Music/"))
         (command "")
         (file-name-template "%(title)s"))
    (setq command (format "yt-dlp -x --embed-metadata --audio-quality 0 --format bestaudio -o '%s%s.%%(ext)s' %s"
                          (file-name-as-directory music-dir) file-name-template url))
    (start-process-shell-command "yt-dlp" "*yt-dlp*" command)
    (message "Downloading audio: %s to %s" url music-dir)))

;; Convert MP4 to WEBM (No Sound)
(defun convert-mp4-to-webm ()
  "Convert file from MP4 to WEBM without audio and move it to ~/Videos/dump/."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (output-dir (expand-file-name "~/Videos/dump/"))
         (output-file (concat output-dir (file-name-base file) ".webm"))
         (command (format "ffmpeg -i \"%s\" -an \"%s\"" file output-file)))
    (shell-command command)
    (message "Converted %s to %s" file output-file)))

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

;; Kensington Orbit scroll utility - change the '11' to the actual ID from 'xinput list' command
(start-process-shell-command
 "xinput" nil "xinput set-prop 12 'libinput Middle Emulation Enabled' 1")
(start-process-shell-command
 "xinput" nil "xinput set-prop 12 'libinput Scroll Method Enabled' 0 0 1")

;; Deluge Daemon
(start-process-shell-command
 "deluged" nil "deluged")
;; Deluge Web Interface
(start-process-shell-command
 "deluge-web" nil "deluge-web")

(provide 'pache-utils)
;;; pache-utils.el ends here


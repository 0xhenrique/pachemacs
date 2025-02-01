;;; pache-media.el --- Music, video etc -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;;; Music Player inside Emacs!
(unless (package-installed-p 'emms)
  (package-install 'emms))

(provide 'pache-media)
;;; pache-media.el ends here

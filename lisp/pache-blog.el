;;; pache-blog --- My blog stuff with Org -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-publish-project-alist
      `(
	("blog"
	 :base-directory "~/workspace/personal/0xhenrique-blog/org-content/"
	 :recursive t
	 :publishing-directory "~/workspace/personal/0xhenrique-blog/content/"
	 :publishing-function org-hugo-export-to-md)))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)

(provide 'pache-blog)
;;; pache-blog.el ends here

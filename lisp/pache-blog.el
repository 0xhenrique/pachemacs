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

;; M-x org-static-blog-create-new-post~ and write the content
;; M-x org-static-blog-publish~ and upload to your webhost
(use-package org-static-blog
  :ensure t
  :config
(setq org-static-blog-publish-title "0xhenrique")
(setq org-static-blog-publish-url "https://0xhenrique.neocities.org/")
(setq org-static-blog-publish-directory "~/workspace/personal/0xhenrique-blog/blog/")
(setq org-static-blog-posts-directory "~/workspace/personal/0xhenrique-blog/posts/")
(setq org-static-blog-drafts-directory "~/workspace/personal/0xhenrique-blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-static-blog-use-preview t)
(setq org-static-blog-preview-ellipsis "(...)")
(setq org-static-blog-preview-link-p t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

  ;; Thanks to https://github.com/lemyx/
  (defun org-static-blog-get-preview (post-filename)
    "Get title, date, tags from POST-FILENAME and get the first paragraph from the rendered HTML.
If the HTML body contains multiple paragraphs, include only the first paragraph,
and display an ellipsis.
Preamble and Postamble are excluded, too."
    (with-temp-buffer
      (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
      (let ((post-title (org-static-blog-get-title post-filename))
            (post-date (org-static-blog-get-date post-filename))
            (preview-region (org-static-blog--preview-region)))
        ;; Put the substrings together.
        (let ((title-link
               (format "<h2 class=\"post-title\"><a href=\"%s\">%s</a></h2>"
                       (org-static-blog-get-post-url post-filename) post-title))
	      (read-article-link
	       (format "<a href=\"%s\">Read more...</a>"
		       (org-static-blog-get-post-url post-filename)))
              (date-link
               (format-time-string (concat "<div class=\"post-date\">"
                                           (org-static-blog-gettext 'date-format)
                                           "</div>")
                                   post-date)))
          (concat
           title-link
           date-link
           preview-region
	   read-article-link
           "<hr class=\"post-divider\">"
           )))))

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"Henrique Marques\">
<meta name=\"referrer\" content=\"no-referrer\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
      "<ul class=\"menu-list\"><li class=\"menu-item\"><a href=\"https://0xhenrique.neocities.org/\">λ 0xhenrique</a></li><li class=\"menu-item\"><a href=\"https://0xhenrique.neocities.org/tag-projects\">Projects</a></li><li class=\"menu-item\"><a href=\"https://0xhenrique.neocities.org/tags\">Tags</a></li><li class=\"menu-item\"><a href=\"https://0xhenrique.neocities.org/archive\">Archive</a></li><li class=\"menu-item\"><a href=\"https://0xhenrique.neocities.org/rss.xml\">RSS</a></li><li class=\"menu-item\" style=\"float:right\"><a href=\"https://0xhenrique.neocities.org/about\">About</a></li></ul>")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
      "<a href=\"https://0xhenrique.neocities.org/rss.xml\">My RSS Feed</a>
<center>Created using GNU Emacs + <a href=\"https://github.com/bastibe/org-static-blog\">Org Static Blog</a>.</center>"))

(provide 'pache-blog)
;;; pache-blog.el ends here

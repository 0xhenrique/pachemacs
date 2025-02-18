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

(setq org-static-blog-publish-title "My Static Org Blog")
(setq org-static-blog-publish-url "https://0xhenrique.neocities.org/")
(setq org-static-blog-publish-directory "~/publog/")
(setq org-static-blog-posts-directory "~/blog/")
(setq org-static-blog-drafts-directory "~/blog/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"John Dow\">
<meta name=\"referrer\" content=\"no-referrer\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">
<link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
      "<div class=\"header\">
  <a href=\"https://staticblog.org\">My Static Org Blog</a>
</div>")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
      "<div id=\"archive\">
  <a href=\"https://staticblog.org/archive.html\">Other posts</a>
</div>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")

;; This HTML code is inserted into the index page between the preamble and
;;   the blog posts
(setq org-static-blog-index-front-matter
      "<h1> Welcome to my blog </h1>\n")

(provide 'pache-blog)
;;; pache-blog.el ends here

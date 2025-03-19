;;; clojure.el --- Functions and Helpers for Clojure/Leiningen Projects -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

(defun pache/leiningen-run-command (command)
  "Run a Leiningen COMMAND in the root of the current project."
  (let ((default-directory (locate-dominating-file default-directory "project.clj")))
    (unless default-directory
      (error "Not inside a Leiningen project"))
    (compile (concat "lein " command))))

(defun pache/leiningen-build ()
  "Run `lein build` in the current Leiningen project."
  (interactive)
  (pache/leiningen-run-command "build"))

(defun pache/leiningen-test ()
  "Run `lein test` in the current Leiningen project."
  (interactive)
  (pache/leiningen-run-command "test"))

(defun pache/leiningen-run ()
  "Run `lein run` in the current Leiningen project."
  (interactive)
  (pache/leiningen-run-command "run"))

(use-package clojure-mode
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c C-b" . pache/leiningen-build)
              ("C-c C-r" . pache/leiningen-run)
              ("C-c C-t" . pache/leiningen-test)))

(provide 'clojure)
;;; clojure.el ends here

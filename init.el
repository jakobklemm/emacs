;; Jakob Klemm - Emacs config

;; Setup straight for installing packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Install use-package for easier install macros
(straight-use-package 'use-package)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Entry point for the emacs config.
;; Most config is not happening in this file, but managed by "setup.el"
;; This file simply installs use-package, which is used in the entire config.

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

;; set package.el repositories
(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; Store scripts and snippets in ressource/ directory
(add-to-list 'load-path "~/.emacs.d/resources/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/resources/")
;; Initial settings, disable some emacs features.
(load-file (concat user-emacs-directory "modules/defaults.el"))
;; Quality of life changes
;; TODO: Bufler 
(load-file (concat user-emacs-directory "modules/qol.el"))
;; Design
(load-file (concat user-emacs-directory "modules/design.el"))
;; Navigation
(load-file (concat user-emacs-directory "modules/navigation.el"))
;; Editor
(load-file (concat user-emacs-directory "modules/editor.el"))
;; Projects
(load-file (concat user-emacs-directory "modules/vcs.el"))
;; Org-mode
(load-file (concat user-emacs-directory "modules/org.el"))
;; Programming
(load-file (concat user-emacs-directory "modules/programming.el"))
;; Communication
(load-file (concat user-emacs-directory "modules/com.el"))
;; Binds
(load-file (concat user-emacs-directory "modules/binds.el"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

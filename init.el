;; Entry point for the emacs config.
;; Most config is not happening in this file, but managed by "setup.el"
;; This file simply installs use-package, which is used in the entire config.

(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; All config and module loading is handled by "setup.el"
(load-file (concat user-emacs-directory "setup.el"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

;; Config structure:
;; setup.el -> Calls out to all required modules and loads them.
;; custom.el -> Added by Custom.
;; defaults.el -> Basic settings, disable & change emacs stuff.
;; qol.el -> Quality of life changes, code snippets, etc. (mostly minor changes)
;; development/major.el -> Entry point for programming major modes.
;; org/org.el -> Entry point for org-mode config, has children.
;; editor -> General editor settings and packages.
;; design.el -> Configures theme, colors, etc.
;; projects/projects.el -> Entry point for project related functions, has children.
;; binds.el -> Setup evil and navigation functions (partially in qol.el)
;; navigation.el -> Helm setup for files, buffers, MX-, etc.

;; Store scripts and snippets in ressource/ directory
(add-to-list 'load-path "~/.emacs.d/resources/")
;; Initial settings, disable some emacs features.
(load-file (concat user-emacs-directory "modules/defaults.el"))
;; Quality of life changes
(load-file (concat user-emacs-directory "modules/qol.el"))
;; Design
(load-file (concat user-emacs-directory "modules/design.el"))
;; Binds
(load-file (concat user-emacs-directory "modules/binds.el"))
;; Navigation
(load-file (concat user-emacs-directory "modules/navigation.el"))
;; Editor
(load-file (concat user-emacs-directory "modules/editor.el"))
;; Projects
(load-file (concat user-emacs-directory "modules/projects.el"))
;; Org-mode
(load-file (concat user-emacs-directory "modules/org.el"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

(use-package quelpa-use-package
  :ensure t)

;; Config structure:

;; Store scripts and snippets in ressource/ directory
(add-to-list 'load-path "~/.emacs.d/resources/")
;; Initial settings, disable some emacs features.
(load-file (concat user-emacs-directory "modules/defaults.el"))
;; Quality of life changes
(load-file (concat user-emacs-directory "modules/qol.el"))
;; Design
(load-file (concat user-emacs-directory "modules/design.el"))
;; Navigation
(load-file (concat user-emacs-directory "modules/navigation.el"))
;; Editor
(load-file (concat user-emacs-directory "modules/editor.el"))
;; Projects
(load-file (concat user-emacs-directory "modules/projects.el"))
;; Org-mode
(load-file (concat user-emacs-directory "modules/org.el"))
;; Programming
(load-file (concat user-emacs-directory "modules/programming.el"))
;; Communication
(load-file (concat user-emacs-directory "modules/com.el"))
;; Binds
(load-file (concat user-emacs-directory "modules/binds.el"))
;; Window manager
;;(load-file (concat user-emacs-directory "modules/windows.el"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(message "Loading completed!")

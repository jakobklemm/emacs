;; Entry point for the emacs config.
;; The actual config is in =config.org= and evaluated here.

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

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

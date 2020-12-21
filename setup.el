;; Config structure:
;; setup.el -> Calls out to all required modules and loads them.
;; custom.el -> Added by Custom.
;; defaults.el -> Basic settings, disable & change emacs stuff.
;; qol.el -> Quality of life changes, code snippets, etc. (mostly minor changes)
;; org/org.el -> Entry point for org-mode config, has children.
;; editor -> General editor settings and packages.
;; design.el -> Configures theme, colors, etc.
 ;; projects/projects.el -> Entry point for project related functions, has children.
;; binds.el -> Setup evil and navigation functions (partially in qol.el)
;; navigation.el -> Helm setup for files, buffers, MX-, etc.

;; Store scripts and snippets in ressource/ directory
(add-to-list 'load-path "~/.emacs.d/resources/")
;; Initial settings, disable some emacs features.
(load-file (concat user-emacs-directory "defaults.el"))
;; Quality of life changes
(load-file (concat user-emacs-directory "qol.el"))
;; Design
(load-file (concat user-emacs-directory "design.el"))
;; Binds
(load-file (concat user-emacs-directory "binds.el"))
;; Navigation
(load-file (concat user-emacs-directory "navigation.el"))
;; Editor
(load-file (concat user-emacs-directory "editor.el"))
;; Projects
(load-file (concat user-emacs-directory "projects/projects.el"))
;; Org-mode
(load-file (concat user-emacs-directory "org/org.el"))
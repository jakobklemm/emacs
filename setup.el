;; Config structure:
;; setup.el -> Calls out to all required modules and loads them.
;; custom.el -> Added by Custom.
;; defaults.el -> Basic settings, disable & change emacs stuff.

(setq user-full-name "Jakob Klemm"
      user-mail-address "jakob@jeykey.net"
      )

;; Store scripts and snippets in ressource/ directory
(add-to-list 'load-path "~/.emacs.d/resources/")

;; Install no-littering to handle all temporary and backup files.
(use-package no-littering
  :ensure t
  )

;; Initial settings, disable some emacs features.
(load-file (concat user-emacs-directory "defaults.el"))

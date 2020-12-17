;; Entry point for the emacs config.
;; Most config is not happening in this file, but managed by "setup.el"
;; This file simply installs use-package, which is used in the entire config.

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))


;; All config and module loading is handled by "setup.el"
(load-file "~/.emacs.d/setup.el")


;; Entry point for emacs config.

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http:///orgmode.org/elpa/") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package)
  )

(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t
      use-package-enable-imenu-support t
      )


(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(perfect-margin use-package dracula-theme auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

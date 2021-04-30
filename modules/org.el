;; Org-mode config

(load-file (concat user-emacs-directory "modules/org-content.el"))

(load-file (concat user-emacs-directory "modules/org-looks.el"))

(load-file (concat user-emacs-directory "modules/org-productivity.el"))

(load-file (concat user-emacs-directory "modules/org-latex.el"))

(load-file (concat user-emacs-directory "hoth/hoth.el"))

(setq
 org-directory "~/documents/"
 initial-buffer-choice  "~/documents/active.org"
 org-archive-location "~/documents/archive/2021.org::* From %s"
 )

(add-hook 'org-mode 'org-toggle-inline-images)
(setq org-image-actual-width '(600))
(setq-default org-display-inline-images t)
(setq-default org-startup-with-inline-images t)

;; Default apps
(setq org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))

;; (use-package org-alert
;;   :ensure t
;;   :config
;;   (setq alert-default-style 'libnotify
;; 	org-alert-interval 600
;; 	org-alert-notification-title "Agenda"
;; 	)
;;   (org-alert-enable)
;;   )

(load "org-recoll")


;; Org-mode tools & extras

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

(use-package disable-mouse
  :ensure t
  :hook (org-mode . disable-mouse-mode))

(load "org-recoll")


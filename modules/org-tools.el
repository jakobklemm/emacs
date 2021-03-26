;; Org-mode tools & extras

(add-hook 'org-mode-hook 'org-toggle-inline-images)
(setq org-image-actual-width '(600))
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-display-remote-inline-images t)
(setq org-startup-with-inline-images t)
(setq org-startup-with-inline-images "inlineimages")

;; Default apps
(setq org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))

(use-package org-alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify
	org-alert-interval 300
	org-alert-notification-title "Agenda"
	)
  (org-alert-enable)
  )

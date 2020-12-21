;; Org-mode config

(load-file (concat user-emacs-directory "org/content.el"))

;; Images
(add-hook 'org-mode-hook 'org-toggle-inline-images)
(setq org-image-actual-width '(600))

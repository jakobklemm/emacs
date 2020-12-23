;; Org-mode config

(load-file (concat user-emacs-directory "modules/org-content.el"))

(load-file (concat user-emacs-directory "modules/org-looks.el"))

(load-file (concat user-emacs-directory "modules/org-productivity.el"))

;; Images
(setq
 org-directory "~/documents/"
 initial-buffer-choice  "~/documents/supervisor/projects.org"
 )
(add-hook 'org-mode-hook 'org-toggle-inline-images)
(setq org-image-actual-width '(600))

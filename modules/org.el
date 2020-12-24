;; Org-mode config

(load-file (concat user-emacs-directory "modules/org-content.el"))

(load-file (concat user-emacs-directory "modules/org-looks.el"))

(load-file (concat user-emacs-directory "modules/org-productivity.el"))

(load-file (concat user-emacs-directory "modules/org-latex.el"))

(setq
 org-directory "~/documents/"
 initial-buffer-choice  "~/documents/supervisor/projects.org"
 )

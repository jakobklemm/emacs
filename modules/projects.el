;; Projects

(use-package projectile
  :commands
  (projectile-ack
   projectile-ag
   projectile-compile-project
   projectile-configure-project
   projectile-package-project
   projectile-install-project
   projectile-test-project
   projectile-run-project
   projectile-dired
   projectile-find-dir
   projectile-find-file
   projectile-find-file-dwim
   projectile-find-file-in-directory
   projectile-find-tag
   projectile-test-project
   projectile-grep
   projectile-invalidate-cache
   projectile-kill-buffers
   projectile-multi-occur
   projectile-project-p
   projectile-project-root
   projectile-recentf
   projectile-regenerate-tags
   projectile-replace
   projectile-replace-regexp
   projectile-run-async-shell-command-in-root
   projectile-run-shell-command-in-root
   projectile-switch-project
   projectile-switch-to-buffer
   projectile-vc
   projectile-commander)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

;; Magit / Version control system
(load-file (concat user-emacs-directory "modules/vcs.el"))

;; Version control system

(use-package magit
  :straight t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x p") 'magit-init)
  (use-package magit-todos
    :straight t
    :config
    (magit-todos-mode t)
    )
  (use-package git-messenger
    :straight t
    :config
    (global-set-key (kbd "C-x m") 'git-messenger)
    )
  )


;; Version control system

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x p") 'magit-init)
  (use-package magit-todos
    :ensure t
    :config
    (magit-todos-mode t)
    )
  (use-package git-messenger
    :ensure t
    )
  )


;; Navigation
(use-package counsel
  :ensure t
  :config
  (use-package swiper
    :ensure t
    )
  (use-package ivy
    :ensure t
    )
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)

  (use-package amx
    :ensure t
    :after ivy
    :custom
    (amx-backend 'auto)
    (amx-save-file "~/.emacs.d/amx-items")
    (amx-history-length 50)
    (amx-show-key-bindings nil)
    :config (amx-mode 1))

  (use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))

  (use-package ivy-rich
    :ensure t
    :after ivy
    :custom (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    :config (ivy-rich-mode 1))
  )

(use-package ivy-posframe
  :ensure t
  :after ivy
  :init (ivy-posframe-mode 1)
  :config (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center))
		ivy-posframe-border-width 12))

(setq ivy-posframe-parameters
      '((left-fringe . 2)
        (right-fringe . 4)))
(setq ivy-posframe-border-width 2)

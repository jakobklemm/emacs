;; Programming

;; Dedicated setups
(load-file (concat user-emacs-directory "modules/elixir.el"))
(load-file (concat user-emacs-directory "modules/rust.el"))


;; General modes

;; General major modes
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  )

(use-package web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook
	    (lambda ()
	      (rainbow-mode)
	      (rspec-mode)
	      (setq web-mode-markup-indent-offset 2)))
  )

(use-package go-mode
  :ensure t
  :config
  (use-package go-errcheck
    :ensure t
    )
  )

(use-package elixir-mode
  :ensure t
  )

(use-package rust-mode
  :ensure t
  )

(use-package markdown-mode
  :custom
  (markdown-hide-markup nil)
  (markdown-bold-underscore t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-indent-function t)
  (markdown-enable-math t)
  (markdown-hide-urls nil)
  :custom-face
  (markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
  (markdown-header-face-1 ((t (:foreground "violet" :weight bold :height 1.0))))
  (markdown-header-face-2 ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
  (markdown-header-face-3 ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
  (markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
  (markdown-list-face ((t (:foreground "mediumpurple"))))
  (markdown-pre-face ((t (:foreground "#bd98fe"))))
  :mode "\\.md\\'")

(use-package markdown-toc
  :ensure t
  )

(use-package logview
  :defer t
  :ensure t
  )

(use-package systemd
  :ensure t
  :mode
  ("\\.service\\'" "\\.timer\\'" "\\.target\\'" "\\.mount\\'"
   "\\.automount\\'" "\\.slice\\'" "\\.socket\\'" "\\.path\\'"
   "\\.netdev\\'" "\\.network\\'" "\\.link\\'"))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :custom-face
  (font-lock-variable-name-face ((t (:foreground "violet")))))

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

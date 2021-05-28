;; Programming

(add-to-list 'exec-path "~/.tools/elixir-ls")

(setq lsp-ui-doc-max-height 52
      lsp-ui-doc-max-width 64
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-show-with-mouse t
      lsp-ui-doc-show-with-cursor t
      )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-auto-activate nil)
  :hook
  (elixir-mode . lsp)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (lsp-ui-doc-enable t)
  (lsp-ui-mode)
  (setq lsp-ui-doc-max-height 128
	lsp-ui-doc-max-width 64
	lsp-ui-doc-position 'top
	lsp-ui-doc-show-with-mouse t
	lsp-ui-doc-show-with-cursor t
      )
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

(use-package elixir-mode
  :ensure t
  )

(use-package rust-mode
  :ensure t
  )

(use-package markdown-mode
  :ensure t
  )

(use-package format-all
  :ensure t
  :bind ("C-c C-f" . format-all-buffer)
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

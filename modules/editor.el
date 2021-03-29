;; Editor

(setq scroll-margin 8)

(use-package smartparens
  :ensure t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package undo-tree
  :ensure t
  :bind
  ("M-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode))

(setq ispell-program-name "hunspell")

(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
        ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_DE" "-a" "-i" "UTF-8") nil utf-8)))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t
    )
  (yas-global-mode 1)
  (setq yas-indent-line 'auto)
  )

(add-to-list 'exec-path "~/.tools/elixir-ls")

(setq lsp-ui-doc-max-height 52
      lsp-ui-doc-max-width 64
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-show-with-mouse t
      lsp-ui-doc-show-with-cursor t
      )

;; company stuff
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.2)
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-posframe
  :ensure t
  :config
  (company-posframe-mode 1))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

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

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol
  )

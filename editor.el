;; Editor

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

;; Completion and corretion (LSP + Hunspell + Flyspell)
(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "~/.emacs.d/dict/en_US.aff")
        ("de_DE" "~/.emacs.d/dict/de_DE.aff")))

(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
        ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_DE" "-a" "-i" "UTF-8") nil utf-8)))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

;; LSP Setup
(use-package lsp-mode
  :commands lsp
  :ensure t
  :diminish lsp-mode
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "~/.emacs.d/elixir-ls"))

;; LSP UI
(use-package lsp-ivy
  :ensure t
  :config
  (setq company-idle-delay 0.2
	lsp-ui-doc-max-height 52
	lsp-ui-doc-max-width 64
	lsp-ui-doc-position 'bottom
	lsp-ui-doc-show-with-mouse t
	lsp-ui-doc-show-with-cursor t
	)
  )

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
  (yas-global-mode 1)
  (setq yas-indent-line 'auto)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)
  )
(global-set-key (kbd "C-x t") 'eshell)

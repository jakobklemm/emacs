;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Initial setup
(setq user-full-name "Jakob Klemm"
      user-mail-address "jakob.klemm@protonmail.com"
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass Mono")
      doom-theme 'doom-vibrant
      load-prefer-newer t
      display-line-numbers-type 'relative
      indent-line-function 'insert-tab
      tab-width 4
      ;; Extra base setup
      delete-by-moving-to-trash t
      window-combination-resize t
      undo-limit 80000000
      auto-save-default t
      history-length 666

      ;; LSP interface config
      company-idle-delay 0.2
      lsp-ui-doc-max-height 52
      lsp-ui-doc-max-width 64
      lsp-ui-doc-position 'bottom
      lsp-ui-doc-show-with-mouse t
      lsp-ui-doc-show-with-cursor t
      company-show-numbers t
      )

;; org-mode configuration
(load-file "~/.doom.d/orgmode.el")

;; Basic LSP setup with elixir-lsp
(use-package! lsp-ui :commands lsp-ui-mode)
(use-package! lsp-ivy :commands lsp-ivy-workspace-symbol)
(add-to-list 'exec-path "~/.doom.d/elixir-ls")
(add-hook 'elixir-mode 'lsp)

;; Spell checks using ispell (external dep)
;;(setq ispell-program-name "hunspell")
;;(setq ispell-hunspell-dict-paths-alist
;;      '(("en_US" "~/.doom.d/dict/en_US.aff")
;;        ("de_DE" "~/.doom.d/dict/de_DE.aff")))

;;(setq ispell-local-dictionary-alist
;;      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
;;        ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_DE" "-a" "-i" "UTF-8") nil utf-8)))

;;(add-hook 'text-mode-hook #'flyspell-mode)
;;(add-hook 'org-mode-hook #'flyspell-mode)

;; Hooks
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook  'auto-fill-mode)
;;(add-hook 'org-mode-hook 'org-toggle-inline-images)
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'org-toggle-inline-images)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; Quality of live changes
;; Start emacs maximized (only relevant for non-tiling use)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Treat camel case names as multiple words.
(global-subword-mode 1)
;; Save the location within a file.
(save-place-mode t)
;; Auto revert (refresh) files
(global-auto-revert-mode t)
;; Set always to UTF-8, only display in bar if not UTF-8
(set-language-environment "UTF-8")
;; Pretty symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("->" . 8594)
          ("<-" . 8592)
          ("<=" . 8804)
          (">=" . 8805)
          ("=~" . 8771)
          ("!=" . 8800)
          (":=" . 8788)
          )
        )
  )

;; Better window switch functionality
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer)
  )
(setq +ivy-buffer-preview t)

;; Specialized sizes & colors, mostly for org-mode.
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "red" :height 0.9 :family "Fira Code")
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  )

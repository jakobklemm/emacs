;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jakob Klemm"
      user-mail-address "jakob.klemm@protonmail.com"
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass Mono")
      doom-theme 'doom-nord
      display-line-numbers-type nil
      load-prefer-newer t
      display-line-numbers-type t
      )

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; org-mode configuration.
(setq org-directory "~/documents/supervisor/"
      org-ellipsis " ▼ "
      org-adapt-indentation t
      indent-line-function 'insert-tab
      tab-width 4
      )

;; Language server protocol - This configures a manual lsp for elixir, because
;; the automatic doom installation is still problematic. Company-Box-Mode is
;; used to display icons in the lsp-dropdown. Next to that lsp is also
;; configured.

(add-hook 'elixir-mode 'lsp)
(add-to-list 'exec-path "~/.doom.d/elixir-ls")

(add-hook 'company-mode-hook 'company-box-mode)

(lsp-ui-mode t)

(setq company-idle-delay nil
      ;;lsp-ui-sideline-show-hover t
      lsp-ui-doc-enable t
      lsp-ui-doc-position 'top
      )

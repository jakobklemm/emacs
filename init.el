;; File executes babel on the real config file config.org.
;; Init package managers
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#172030" "#c6797e" "#A3B09A" "#F7E3AF" "#6e94b9" "#b18bb1" "#88C0D0" "#FAFFF6"])
 '(custom-safe-themes
   '("43cadc6254cf27ff544e044b4139a7d50cf44e107cffef255aa8c5943581f606" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "2899018e19d00bd73c10c4a3859967c57629c58a955a2576d307d9bdfa2fea35" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" default))
 '(fci-rule-color "#64727d")
 '(jdee-db-active-breakpoint-face-colors (cons "#070A0E" "#B16E75"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#070A0E" "#A3B09A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#070A0E" "#64727d"))
 '(lsp-ui-doc-position 'bottom)
 '(objed-cursor-color "#c6797e")
 '(org-agenda-files '("~/private/documents/main.org" "~/.emacs.d/config.org"))
 '(package-selected-packages
   '(org-super-agenda tangotango-theme rainbow-mode counsel doom-modeline deft org-gcal minimap doom-themes elixir-yasnippets yasnippet company swiper flycheck ivy rainbow-delimiters malyon org-special-block-extras ox-mdx-deck dimmer magit smartparens lsp-ui elixir-mode eglot company-ghc company-jedi company-erlang dashboard projectile page-break-lines minions moody use-package babel auto-compile))
 '(pdf-view-midnight-colors (cons "#FAFFF6" "#172030"))
 '(rustic-ansi-faces
   ["#172030" "#c6797e" "#A3B09A" "#F7E3AF" "#6e94b9" "#b18bb1" "#88C0D0" "#FAFFF6"])
 '(vc-annotate-background "#172030")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3B09A")
    (cons 40 "#bfc1a1")
    (cons 60 "#dbd2a8")
    (cons 80 "#F7E3AF")
    (cons 100 "#f2d6a8")
    (cons 120 "#eecaa1")
    (cons 140 "#eabe9a")
    (cons 160 "#d7ada1")
    (cons 180 "#c39ba9")
    (cons 200 "#b18bb1")
    (cons 220 "#b884a0")
    (cons 240 "#bf7f8f")
    (cons 260 "#c6797e")
    (cons 280 "#ad777d")
    (cons 300 "#95757d")
    (cons 320 "#7c737d")
    (cons 340 "#64727d")
    (cons 360 "#64727d")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
 '(cfw:face-day-title ((t :background "grey10")))
 '(cfw:face-default-content ((t :foreground "#bfebbf")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
 '(cfw:face-grid ((t :foreground "DarkGrey")))
 '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
 '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
 '(cfw:face-periods ((t :foreground "cyan")))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
 '(cfw:face-select ((t :background "#2f2f2f")))
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
 '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
 '(cfw:face-today ((t :background: "grey10" :weight bold)))
 '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
 '(cfw:face-toolbar ((t :foreground "#ff1111" :background "#ff1111")))
 '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
 '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold)))
 '(markdown-code-face ((t nil))))

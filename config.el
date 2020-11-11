;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Base config
(setq user-full-name "Jakob Klemm"
      user-mail-address "jakob.klemm@protonmail.com"
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass Mono")
      doom-theme 'doom-nord
      display-line-numbers-type nil
      load-prefer-newer t
      display-line-numbers-type t
      org-pretty-entities t
      browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome-stable"
      )

(set-language-environment "UTF-8")

;; org-mode configuration.
(setq org-directory "~/documents/supervisor/"
      org-ellipsis " ▼ "
      org-adapt-indentation t
      indent-line-function 'insert-tab
      tab-width 4
      org-startup-folded t
      org-src-fontify-natively t
      org-priority-faces '(
                           (?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))
                           )
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t
      )

(let*
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.40))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.55))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "dark orange" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; Superstar mode for better symbols (replacements for org-bullets)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(org-superstar-configure-like-org-bullets)
(setq org-superstar-special-todo-items t
      org-superstar-todo-bullet-alist '(
                                        ("TODO" . 9744)
                                        ("BLOCKED" . 9202)
                                        ("PROGRESS" . 8885)
                                        ("DONE" . 9745)
                                        ("PAL" . 9745)
                                        ("IDEA" . 1422)
                                        ("NOTE" . 9738)
                                        ("INTAKE" . 8227)
                                        )
     )

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook  'auto-fill-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'after-init-hook 'org-roam-mode)

;; Pretty symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  '(
    ("lambda" . 955)
    ("->" . 8594)
    ("<-" . 8592)
    ("<=" . 8804)
    (">=" . 8805)
    ("=~" . 8771)
    ("!=" . 8800)
    )
  )

;; Treat camel case names as multiple words.
(global-subword-mode 1)

;; Save the location within a file.
(save-place-mode t)
;; Auto revert (refresh) files
(global-auto-revert-mode t)

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

;; org-roam && org-roam-server
(setq org-roam-db-location "~/documents/vaults/org-roam.db"
      org-roam-capture-templates '(
                                   ("i" "With fixed filename, for export to hugo." plain (function org-roam--capture-get-point)
                                    "%?"
                                    :file-name "${slug}"
                                    :head "#+title: ${title}\n#+SETUPFILE:~/.hugo.org\n"
                                    :unnarrowed t)
                                   ("o" "Unchanged org-roam insert." plain (function org-roam--capture-get-point)
                                    "%?"
                                    :head "#+title: ${title}\n")
                                   )
      org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-export-inline-images t
      org-roam-server-serve-files nil
      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "jpg" "png")
      org-roam-server-network-poll t
      org-roam-server-network-arrows nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20
      )

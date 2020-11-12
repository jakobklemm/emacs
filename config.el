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
      browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome-stable"
      company-idle-delay 0
      )

(set-language-environment "UTF-8")

;; org-mode configuration.
(after! org (setq org-directory "~/documents/supervisor/"
                  org-ellipsis " ▼ "
                  org-adapt-indentation t
                  indent-line-function 'insert-tab
                  tab-width 4
                  org-pretty-entities t
                  org-startup-folded t
                  org-src-fontify-natively t
                  org-priority-faces '(
                                       (?A . (:foreground "red" :weight 'bold))
                                       (?B . (:foreground "yellow"))
                                       (?C . (:foreground "green"))
                                       )
                  org-src-tab-acts-natively t
                  org-hide-emphasis-markers t
                  org-src-window-setup 'current-window
                  org-return-follows-link t
                  org-confirm-babel-evaluate nil
                  org-use-speed-commands t
                  org-catch-invisible-edits 'show
                  org-todo-keywords '(
                                      (sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "PAL(p)")
                                      (sequence "NOTE(m)" "BLOCKED(b)" | "DONE(d)")
                                      )
                  org-refile-use-outline-path 'file
                  org-tag-alist '(
                                  ("@event" . ?e)
                                  ("@private" . ?p)
                                  ("@schule" . ?s)
                                  )
                  org-refile-allow-creating-parents-nodes 'confirm
                  org-refile-targets '((org-agenda-files . (:level . 1)))
                  org-log-done 'time
                  org-log-into-drawer t
                  org-capture-templates `(
                                          ("i" "Inbox" entry (file "~/documents/supervisor/tasks.org"))
                                          )
                  )
  )

;; Superstar mode for better symbols (replacements for org-bullets)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(after! org (setq org-superstar-special-todo-items t
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

(use-package! lsp-mode
  :hook (elixir-mode . lsp)
  :commands lsp
  )

(use-package! lsp-ui :commands lsp-ui-mode)
(use-package! lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Start emacs maximized (only relevant for non-tiling use)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; org-roam && org-roam-server
(setq org-roam-db-location "~/documents/vaults/org-roam.db"
      org-roam-directory "~/documents/vaults/knowledge/"
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

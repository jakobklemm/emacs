;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Base settings and configuration
(setq user-full-name "Jakob Klemm"
      user-mail-address "jakob.klemm@protonmail.com"
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Overpass Mono")
      doom-theme 'doom-vibrant
      load-prefer-newer t
      display-line-numbers-type 'relative
      browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome-stable"
      delete-by-moving-to-trash t
      window-combination-resize t
      undo-limit 80000000
      evil-want-fine-undo t
      auto-save-default t
      evil-vsplit-window-right t
      evil-split-window-below t
      +ivy-buffer-preview t
      history-length 442
      )

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
                  ;;org-log-done 'time
                  org-log-into-drawer t
                  org-capture-templates `(
                                          ("i" "Inbox" entry (file "~/documents/supervisor/tasks.org"))
                                          )
                  org-use-property-inheritance t
                  org-catch-invisible-edits 'smart
                  ;; Agenda
                  org-agenda-skip-scheduled-if-done t
                  org-agenda-skip-deadline-if-done t
                  org-agenda-include-deadlines t
                  org-agenda-block-separator nil
                  org-agenda-tags-column 130
                  org-agenda-compact-blocks t
                  org-agenda-deadline-faces '(
                                              (1.001 . error)
                                              (1.0 . org-warning)
                                              (0.5 . org-upcoming-deadline)
                                              (0.0 . org-upcoming-distant-deadline)
                                              )
                  org-fontify-quote-and-verse-blocks t
                  org-agenda-custom-commands '(
                                               (
                                                "o" "Overview" (
                                                                (agenda ""
                                                                        (
                                                                         (org-agenda-span 'day)
                                                                         (org-super-agenda-groups
                                                                          '(
                                                                            (:name "Today"
                                                                             :time-grid t
                                                                             :date today
                                                                             :todo "TODAY"
                                                                             :scheduled today
                                                                             :order 1
                                                                             )
                                                                            )
                                                                          )
                                                                         )
                                                                        )
                                                                )
                                                )
                                               )
                  )
  )

(use-package! org-super-agenda
  :commands (org-super-agenda-mode)
  )
(after! org-agenda
  (org-super-agenda-mode)
  )

(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook  'auto-fill-mode)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
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

(use-package! org-ref
  :after org
  :config
  (setq org-ref-completion-library 'org-ref)
  )

;; Set always to UTF-8, only display in bar if not UTF-8
(set-language-environment "UTF-8")

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer)
  )

(defun modeline-conditional-encoding ()
  "Mostly UTF-8 so only show if not that."
  (setq-local doom-modeline-buffer-encoding
              (unless
                  (or
                   (eq buffer-file-coding-system 'utf-8-unix)
                   (eq buffer-file-coding-system 'utf-8)
                   )
                  )
              )
  )

(add-hook 'after-change-major-mode-hook #'modeline-conditional-encoding)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "red" :height 0.9 :family "Fira Code")
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  )

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.2)
    )
  )

(after! org
  (use-package! org-pretty-tags
    :config
    (setq
     org-pretty-tags-surrogate-strings `(
                                         ("schule" . ,(all-the-icons-faicon "graduation-cap" :face 'all-the-icons-purple :v-adjust 0.01))
                                         )
     )
    )
  )

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
  :config (setq
           lsp-ui-doc-max-height 52
           lsp-ui-doc-max-width 64
           lsp-ui-doc-position 'bottom
           lsp-ui-doc-show-with-mouse t
           lsp-ui-doc-show-with-cursor t
           company-idle-delay 0.25
           company-show-numbers t
           )
  )

(use-package! lsp-ui :commands lsp-ui-mode)
(use-package! lsp-ivy :commands lsp-ivy-workspace-symbol)

;; Company config
;; Setup all company-backends
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode
    )
  '(:seperate
    company-ispell
    company-files
    company-yasnippet
    )
 )

;; Start emacs maximized (only relevant for non-tiling use)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; org-mode export & LaTeX config
(setq
 org-confirm-babel-evaluate t
 org-latex-listings 'listed
 TeX-parse-self t
 TeX-PDF-mode t
 )

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

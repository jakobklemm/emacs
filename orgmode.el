;;; orgmode.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/documents/"
        org-ellipsis " ▼ "
        org-adapt-indentation nil
        org-fontify-quote-and-verse-blocks t
        org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow))
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-src-window-setup 'current-window
        org-return-follows-link t
        org-confirm-babel-evaluate nil
        org-use-speed-commands t
        org-startup-folded t
        org-agenda-start-on-weekday nil
        org-agenda-start-day "0d"
        org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-compact-blocks t
        org-agenda-start-with-log-mode t
        org-refile-use-outline-path 'file
        org-catch-invisible-edits 'show
        org-image-actual-width '(600)
        org-log-done 'time
        org-archive-location "~/documents/vaults/archive/archive.org::* From %s"
        org-refile-targets '(;;("~/documents/supervisor/projects.org" :maxlevel . 3)
                             ;;("~/documents/supervisor/last.org" :maxlevel . 1)
                             ;;("~/documents/supervisor/inbox.org" :maxlevel . 3)
                             ;;("~/documents/supervisor/areas.org" :maxlevel . 3)
                             ;;("~/documents/supervisor/events.org" :maxlevel . 1)
                             ("~/documents/supervisor/gsd.org" :maxlevel . 1)
                             )
        org-capture-templates '(("c" "Inbox TODO" entry (file "~/documents/supervisor/inbox.org")
                                 "* TODO %?\n  %i\n  %a")
                                )
        )
  )

(defun my/refile ()
  (interactive)
    (org-refile nil nil (list "gsd.org" (find-file "~/documents/supervisor/gsd.org") nil (point)))
  )

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.2))
  (follow-mode)
  (load-library "find-lisp")
  (setq org-agenda-files
        (find-lisp-find-files "~/documents/supervisor/" "\.org$"))
  )

(after! org-superstar
  (setq ;;org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
   org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
   org-superstar-prettify-item-bullets t
   ;; Enable custom bullets for TODO items
   org-superstar-special-todo-items t
   org-superstar-todo-bullet-alist '(("TODO" "☐ ")
                                     ("NEXT" "✒ ")
                                     ("STATIC" "» ")
                                     ("BLOCKED" "˧ ")
                                     ("DONE" "✔ ")
                                     ("PAL" "✔ ")
                                     )
   )
  (org-superstar-restart))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

;; org-mode TODO keywords and functions
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "STATIC(s)" "BLOCKED(b)" "|" "PAL(p)"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "#af1212" :weight bold))
          ("NEXT" . (:foreground "#a8fa80" :weight bold))
          ("BLOCKED" . (:foreground "#b213c4" :weight bold))
          ("PAL" . (:foreground "#30bb03" :weight bold))
          ("STATIC" . (:foreground "#eaa222" :weight bold))
          ("DONE" . (:foreground "#ffffff" :weight bold))
          ))
  )

(after! org
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                             :todo "NEXT"
                             :order 1)))))
            (agenda "" ((org-agenda-span 4)
                        (org-super-agenda-groups
                         '((:name "Week"
                            :time-grid t
                            :date today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                             :todo "NEXT"
                             :order 1)

                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :face error
                             :order 7)
                            (:name "Trivial"
                             :priority<= "E"
                             :tag ("Trivial" "Unimportant")
                             :todo ("SOMEDAY" )
                             :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

  )

(defun todo/done ()
  (interactive)
  (org-todo 'done))

;; Org binds
(map! :leader
      (:prefix ("a" . "applications")
       :desc "Org capture" "c" #'org-capture
       :desc "Setting org-mode tags" "t" #'org-set-tags-command
       :desc "Org-todo menu" "f" #'org-todo
       :desc "Org-todo mark as done" "x" #'todo/done
       :desc "Bring an org-mode file to agenda from" "v" #'org-agenda-file-to-front
       :desc "Refile org-heading, used in supervisor" "r" #'org-refile
       )
      )

;; org-mode export
(setq
 org-html-postamble nil
 TeX-parse-self t
 TeX-PDF-mode t
 )

(after! org
  )

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-math-mode)
	    (setq TeX-master t)))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode 1)))

;; inside .emacs file
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(after! org
  '(progn
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "google-chrome-stable %s")))

(use-package! ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)

(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :latex_class   "🄲"
              :latex_header  "⇥"
              :beamer_header "↠"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :begin_quote   "❮"
              :end_quote     "❯"
              :caption       "☰"
              :header        "›"
              :begin_export  "⯮"
              :end_export    "⯬"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :title         "#+TITLE:"
    :subtitle      "#+SUBTITLE:"
    :author        "#+AUTHOR:"
    :date          "#+DATE:"
    :property      "#+PROPERTY:"
    :options       "#+OPTIONS:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_latex:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"))
(plist-put +ligatures-extra-symbols :name "⁍")

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  (setq org-roam-directory (file-truename "~/documents/database")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("f" "unchanged" plain (function org-roam-capture--get-point)
           "%?"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)))
  )

;; org-roam-server
(setq org-roam-db-location "~/documents/vaults/org-roam.db"
      org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-export-inline-images t
      org-roam-server-serve-files t
      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "jpg" "png")
      org-roam-server-network-poll t
      org-roam-server-network-arrows nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20
      )

(after! org-ref
  (setq org-ref-default-bibliography '("~/documents/vaults/biblio.bib")))

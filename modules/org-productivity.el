;; Org-mode productivity
;; Agenda, Tasks, Refile, Capture

(setq
 org-log-done 'time
 org-todo-keywords
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

(defun agenda/super (&optional arg)
  (interactive "P")
  (org-agenda arg "d"))

;; Make agenda a global keybind.
(global-set-key (kbd "C-c  a") 'org-agenda)
;; Direct access to super-agenda
(global-set-key [f1] 'agenda/super)

(define-key org-mode-map (kbd "C-c d") 'org-deadline)
(define-key org-mode-map (kbd "C-c s") 'org-schedule)
(define-key org-mode-map (kbd "C-c e") 'org-set-effort)

(defun todo/done ()
  (interactive)
  (org-todo 'done))

(defun todo/todo  ()
  (interactive)
  (org-todo 'todo)
  (org-priority-up)
  )

(defun home-file ()
    (interactive)
    (find-file "~/documents/supervisor/gsd.org")
    )

(use-package org-super-agenda
  :ensure t
  :init
  (setq org-agenda-custom-commands
        '(("s" "Super Agenda - Week"
           ((agenda "" ((org-agenda-span 'week)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "Ivy-Lee")
                         (org-agenda-files '("~/documents/supervisor/gsd.org"))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo ("TODO" "NEXT" "STATIC" "BLOCKED")
                             :order 2)
                            (:discard (:anything))
                            ))))
            (alltodo "" ((org-agenda-overriding-header "Next tasks")
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "NEXT"
                             :order 4)
                            (:discard (:anything))
                            ))))
            (alltodo "" ((org-agenda-overriding-header "All items")
                         (org-super-agenda-groups
                          '((:name ""
                             :todo ("TODO" "NEXT" "STATIC" "BLOCKED")
                             :order 5)
                            (:discard (:anything))
                            ))))
            )
           )
          ("d" "Super Agenda - Day"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "Ivy-Lee")
                         (org-agenda-files '("~/documents/supervisor/gsd.org"))
                         (org-super-agenda-groups
                          '((:name ""
                             :todo ("TODO" "NEXT" "STATIC" "BLOCKED")
                             :order 2)
                            (:discard (:anything))
                            ))))
            (alltodo "" ((org-agenda-overriding-header "Next tasks")
                         (org-super-agenda-groups
                          '((:name ""
                             :todo "NEXT"
                             :order 4)
                            (:discard (:anything))
                            ))))
            (alltodo "" ((org-agenda-overriding-header "All items")
                         (org-super-agenda-groups
                          '((:name ""
                             :todo ("TODO" "NEXT" "STATIC" "BLOCKED")
                             :order 5)
                            (:discard (:anything))
                            ))))
            )
           )
          )
        )
  :config
  (org-super-agenda-mode 1)
  )

(setq
 org-agenda-start-on-weekday nil
 org-agenda-start-day "0d"
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-include-deadlines t
 org-agenda-current-time-string "← now"
 )

;; https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun ivy/refile-to (file headline)
  "Move current headline to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

(defun ivy/refile ()
  "Move current headline to bookmarks"
  (interactive)
  (org-mark-ring-push)
  (ivy/refile-to "~/documents/supervisor/gsd.org" "gsd.org")
  (org-mark-ring-goto))

(defun ivy/last ()
  "Move current headline to bookmarks"
  (interactive)
  (org-mark-ring-push)
  (ivy/refile-to "~/documents/supervisor/last.org" "Week")
  (org-mark-ring-goto))



(setq
 org-refile-targets '(("~/documents/supervisor/projects.org" :maxlevel . 3)
		      ("~/documents/supervisor/last.org" :maxlevel . 1)
		      ;;("~/documents/supervisor/inbox.org" :maxlevel . 3)
		      ;;("~/documents/supervisor/areas.org" :maxlevel . 3)
		      ;;("~/documents/supervisor/events.org" :maxlevel . 1)
		      ("~/documents/supervisor/gsd.org" :maxlevel . 1)
		      )
 )

(setq
 org-capture-templates '(("c" "Inbox TODO" entry (file "~/documents/supervisor/inbox.org")
                          "* TODO %?\n  %i\n  %a")
			 ("t" "Table" table-line
			  (file+headline "~/documents/supervisor/content.org" "Books")
			  "| %? |  |  |  |")
                         )
 org-tag-alist '(("@NEXT" . ?n) ("@home" . ?h) ("laptop" . ?l))
 )

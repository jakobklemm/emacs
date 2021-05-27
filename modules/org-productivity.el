
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
        )
      org-capture-templates '(("x" "Inbox TODO" entry (file "~/documents/aggregation.org")
                               "* TODO %?\n  %i\n  %a")
			      ("c" "Common" entry (file+headline "~/documents/active.org" "Common")
			       "* TODO %?\n%U\n   %c" :empty-lines 1)
                              )

      org-tag-alist '(("drill" . ?d))
      org-refile-targets '(("~/documents/active.org" :maxlevel . 1)
			   ("~/documents/completed.org" :maxlevel . 1)
			   )
      )

(defun agenda/super (&optional arg)
  (interactive "P")
  (org-agenda arg "d"))

;; Direct access to super-agenda
(global-set-key [f1] 'agenda/super)

(defun todo/done ()
  (interactive)
  (org-todo 'done))

(defun todo/todo  ()
  (interactive)
  (org-todo "NEXT")
  (org-mark-ring-push)
  (ivy/refile-to "~/documents/active.org" "Today")
  (org-mark-ring-goto)
  ;;(org-priority-up)
  ;;(org-deadline nil (org-read-date nil nil "+1d"))
  )

(defun home-file ()
    (interactive)
    (find-file "~/documents/active.org")
    )

(defun projects-file ()
    (interactive)
    (find-file "~/documents/aggregation.org")
    )

(use-package org-super-agenda
  :straight t
  :init
  (setq org-agenda-custom-commands
        '(("d" "Super Agenda - Day"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
				  :time-grid t
				  :date today
				  :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "Next")
			 (org-agenda-files '("~/documents/active.org"))
                         (org-super-agenda-groups
                          '((:name ""
				   :todo "NEXT"
				   :order 1)
                            (:discard (:anything))
                            ))))
	    (alltodo "" ((org-agenda-overriding-header "Projects")
			 (org-agenda-files '("~/documents/active.org"))
                         (org-super-agenda-groups
                          '((:name ""
				   :todo ("TODO" "STATIC" "BLOCKED")
				   :order 2)
                            (:discard (:anything))
                            )
                            )))
            (alltodo "" ((org-agenda-overriding-header "Other")
                         (org-super-agenda-groups
                          '((:name ""
				   :file-path "aggregation"
				   :order 5)
                            (:discard (:anything t)))
                            )))
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
  (ivy/refile-to "~/documents/active.org" "Today")
  (org-mark-ring-goto))

(defun ivy/last ()
  "Move current headline to bookmarks"
  (interactive)
  (org-mark-ring-push)
  (ivy/refile-to "~/documents/completed.org" "Week")
  (org-mark-ring-goto))

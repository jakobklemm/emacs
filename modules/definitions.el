;; Definitions

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

;; Quick access (selection)
(my-leader-def
  :keymaps 'normal
  "a" 'agenda/super
  "d" 'ivy/refile
  "h" 'home-file
  "c" 'org-capture
  "x" 'todo/done
  )

;; Buffers
(my-leader-def
  :keymaps 'normal
  "bs" 'save-buffer
  "bk" 'kill-current-buffer
  "bj" 'kill-buffer-and-window
  "bb" 'ivy-switch-buffer
  "bf" 'find-file
  )

;; Windows & Navigation
(my-leader-def
  :keymaps 'normal
  "wv" 'evil-window-vsplit
  "wj" 'kill-buffer-and-window
  "w1" 'delete-other-windows
  "w2" 'hrs/split-window-below-and-switch
  "w3" 'hrs/split-window-right-and-switch
  "w4" 'find-file-other-window
  "wk" 'kill-current-buffer
  "wo" 'ace-window
  "wj" 'kill-buffer-and-window
  "wg" 'dumb-jump-go-other-window
  "wh" 'dump-jump-go
  "wb" 'dumb-jump-back
  )

;; Files
(my-leader-def
  :keymaps 'normal
  "ff" 'find-file
  "fr" 'recentf-open-most-recent-file
  "fb" 'counsel-bookmark
  )

;; Editing & Text
(my-leader-def
  :keymaps 'normal
  "ö" 'ivy-immediate-done
  "p" 'ivy-yank-word
  "," 'set-mark-command
  "." 'kill-ring-save
)

;; Search
(my-leader-def
:keymaps 'normal
"ss" 'swiper
"sr" 'replace-string
)

;; Admin
(my-leader-def
  :keymaps 'normal
  "qq" 'save-buffers-kill-terminal
  "qv" 'emacs-version
  "qi" 'emacs-init-time
  "qu" 'emacs-uptime
  "qe" 'eshell
  )

;; Org-mode
(my-leader-def
  :keymaps 'normal
  "oi" 'org-cycle
  "oa" 'org-agenda
  "oc" 'org-capture
  "od" 'org-deadline
  "os" 'org-schedule
  "ot" 'org-todo
  "or" 'org-set-tags-command
  "oe" 'org-set-effort
  "ox" 'todo/done
  "oh" 'home-file
  "og" 'ivy/refile
  "ol" 'org-insert-link
  "oö" 'org-store-link
  "oo" 'org-open-at-point
  "op" 'org-link-open-as-file
  )

;; Magit & VCS
(my-leader-def
  :keymaps 'normal
  "gg" 'magit-status
  "gi" 'magit-init
  "gm" 'git-messenger:popup-message
  "gp" 'magit-pull
  )

;; Org-roam
(my-leader-def
  :keymaps 'normal
  "nl" 'org-roam
  "ni" 'org-roam-insert
  "nf" 'org-roam-find-file
  "nc" 'org-roam-capture
  "nr" 'org-roam-random-note
  )

;; Email / Com
(my-leader-def
 :keymaps 'normal
  "mo" 'mu4e
  "mc" 'mu4e-compose-new
  "mm" 'message-send-and-exit
  "ma" 'mail-add-attachment
  "ms" 'mml-secure-message-sign-pgp
  "me" 'mml-secure-message-encrypt-pgp
  "mj" 'mu4e~headers-jump-to-maildir
  )

;; Definitions

(global-set-key (kbd "C-x j") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "<f5>") 'home-file)
(global-set-key (kbd "<f6>") 'projects-file)

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(when (string-equal system-name "jeykeyarch")
  (general-def 'normal
    ;; Movement 1
    "h" 'evil-next-line
    "t" 'evil-previous-line
    "d" 'evil-backward-char
    "n" 'evil-forward-char
    "N" 'evil-scroll-page-down
    "D" 'evil-scroll-page-up

    ;;  Movement 2
    "l" 'evil-forward-word-begin
    "L" 'evil-forward-WORD-begin
    "g" 'evil-backward-word-end
    "G" 'evil-backward-WORD-end
    "f" 'evil-first-non-blank
    "r" 'evil-end-of-line

    ;; Editing
    "m" 'evil-insert
    "w" 'evil-append
    "b" 'evil-delete-char
    "v" 'evil-delete-line

    ;; Newline
    "s" 'evil-open-below
    "S" 'evil-open-above

    "p" 'ivy/refile
    "y" 'ivy/last
    ";" 'agenda/super
    "," 'todo/todo
    "." 'todo/done

    "k" 'org-capture
    "c" 'org-deadline
    "x" 'org-schedule

    )
  )

(when (string-equal system-name "86d38172")
  ;; Quick access (selection)
  (my-leader-def
    :keymaps 'normal
    "a" 'agenda/super
    "e" 'ivy/refile
    "r" 'ivy/last
    "t" 'todo/todo
    "d" 'org-deadline
    "s" 'org-schedule
    "h" 'home-file
    "j" 'projects-file
    "c" 'org-capture
    "x" 'todo/done
  )

  )

;; Buffers
(my-leader-def
  :keymaps 'normal
  "bs" 'save-buffer
  "bk" 'kill-current-buffer
  "bj" 'kill-buffer-and-window
  "bb" 'ivy-switch-buffer
  "bf" 'find-file
  "bh" 'previous-buffer
  "bl" 'next-buffer
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
  "," 'find-file
  "." 'ivy-switch-buffer
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
)

;; Search
(my-leader-def
  :keymaps 'normal
  "ys" 'swiper
  "yr" 'replace-string
  "yf" 'org-recoll-search
  "yu" 'org-recoll-update-index
  "yeg" 'engine/search-google
  "yep" 'engine/search-duckduckgo
  "yed" 'engine/search-github
  "yee" 'engine/search-prompt
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
  "oz" 'org-set-tags-command
  "oe" 'org-set-effort
  "ox" 'todo/done
  "or" 'org-refile
  "og" 'ivy/refile
  "ob" 'ivy/last
  "ol" 'org-insert-link
  "oö" 'org-store-link
  "oo" 'org-open-at-point
  "op" 'org-link-open-as-file
  "of" 'org-agenda-file-to-front
  "ow" 'org-export-dispatch
  "oh" 'hoth-total
  "oy" 'org-archive-subtree
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
  "ns" 'org-roam-server-mode
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
  "ml" 'mu4e~view-browse-url-from-binding
  "mf" 'mu4e~view-save-attach-from-binding
  )

;; EXWM Navigation
;;(define-key global-map [?\s] nil)
(general-define-key
 :prefix "s-SPC"
 ;; bind "C-c a" to 'org-agenda
 "b" 'ido-switch-buffer
 "f" 'ido-find-file
 "w"  'exwm-workspace-switch
 )

(general-define-key
 :prefix "s-M"
 ;; bind "C-c a" to 'org-agenda
 "b" 'ido-switch-buffer
 "f" 'ido-find-file
 "w"  'exwm-workspace-switch
 )

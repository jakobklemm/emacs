;; Evil binds & navigation
;; http://evgeni.io/posts/quick-start-evil-mode/

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package evil
  :ensure t
  :init
  (setq evil-move-beyond-eol t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)

  (use-package evil-commentary
    :ensure t
    :bind (:map evil-normal-state-map
                ("gc" . evil-commentary)))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    )

  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init)
    )
  )

(use-package general
  :ensure t
  )

;; Keybind config
;; structure partially copied from general.el readme.
(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

;; Quick access (selection)
(my-leader-def
  :keymaps 'normal
  "a" 'org-agenda
  "s" 'agenda/super
  "d" 'ivy/refile
  "h" 'home-file
  "c" 'org-capture
  "x" 'todo/done
  )

;; Buffers
(my-leader-def
  :keymaps 'normal
  "bs" 'save-buffer
  "bk" 'kill-buffer
  "bj" 'kill-buffer-and-window
  "bb" 'ivy-switch-buffer
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
  "wj" 'dumb-jump-go-other-window
  "wh" 'dump-jump-go
  "wb" 'dumb-jump-back
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
  "mn" 'compose-mail-other-window
  "mm" 'message-send-and-exit
  "ms" 'mml-secure-message-sign-pgp
  )

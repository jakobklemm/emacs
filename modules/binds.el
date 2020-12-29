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
  "c" 'org-capture)

;; Buffers

(my-leader-def
  :keymaps 'normal
  "bs" 'save-buffer
  "bk" 'kill-buffer
  "bj" 'kill-buffer-and-window
  "bb" 'ivy-switch-buffer
  )

;; Org-mode
;; Org-mode navigation
(general-define-key
 :keymaps 'org-mode-map
 "gj" 'org-next-visible-heading
 "gp" 'org-previous-visible-heading
 "gi" 'org-cycle
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

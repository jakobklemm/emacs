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
  (setq evil-want-integration t) ;; required by evil-collection
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

(use-package evil-mu4e
  :ensure t
  )

;;(evil-set-initial-state 'pdf-view-mode 'normal)
;;(evil-set-initial-state 'image-mode 'normal)

;; Definitions: define keys and functions
;; Undefinitions: remove common emacs binds for easier learning.

(load-file (concat user-emacs-directory "modules/definitions.el"))

(load-file (concat user-emacs-directory "modules/undefinitions.el"))

;; Definitions

(global-set-key (kbd "C-x j") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "<f5>") 'home-file)
(global-set-key (kbd "<f6>") 'projects-file)


;; Partially copied from https://github.com/jbranso/evil-dvorak/blob/master/evil-dvorak.el

(define-minor-mode dvorak-mode
  "Evil dvorak mode allows you to use evil using the dvorak keyboard layout.  Contributions are welcome."
  nil
  :global t
  :lighter " ED"
  :keymap (make-sparse-keymap))

(defun turn-on-dvorak-mode ()
  "Enable evil-dvorak-mode in the current buffer."
  (dvorak-mode 1))

(defun turn-off-dvorak-mode ()
  "Disable evil-dvorak-mode in this buffer."
  (dvorak-mode -1))

(define-globalized-minor-mode global-dvorak-mode
  dvorak-mode turn-on-dvorak-mode
  "Global mode to let you use evil with dvorak friendly keybindings.")

(global-dvorak-mode 1)

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(evil-define-key 'motion org-agenda-mode-map
  "h" 'org-agenda-earlier
  "l" 'org-agenda-later
  "j" 'org-agenda-next-line
  "k" 'org-agenda-previous-line
  "ö" 'org-agenda-goto-today
  ) 

(when (string-equal system-name "jeykeyarch")
  (evil-define-key '(visual normal motion) dvorak-mode-map
    "t" 'evil-next-line
    "n" 'evil-previous-line
    "h" 'evil-backward-char
    "s" 'evil-forward-char

    "H" 'evil-backward-word-begin
    "S" 'evil-forward-word-end
    "T" 'evil-scroll-page-down
    "N" 'evil-scroll-page-up
    
    "l" 'evil-first-non-blank
    "r" 'evil-end-of-line
    "g" 'backward-paragraph
    "q" 'forward-paragraph

    "m" 'evil-insert
    "z" 'evil-open-below
    "v" 'evil-delete-char
    "w" 'kill-line

    "M" 'evil-append
    "Z" 'evil-open-above
    "V" 'kill-word
    "W" 'kill-comment
    
    "f" 'yank
    "d" 'undo
    "b" 'kill-ring-save

    "p" 'ivy/refile
    "y" 'ivy/last

    ";" 'agenda/super
    "," 'todo/done
    "." 'todo/done

    ;; "Temporary quick binds.
    "j" 'kill-buffer-and-window
    "'" 'mu4e-headers-search-bookmark
    "a" 'find-file

    ;; "c" 'org-capture
    ;; "k" 'org-schedule
    ;; "x" 'org-deadline
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
    "k" 'org-schedule
    "h" 'home-file
    "j" 'projects-file
    "c" 'org-capture
    "x" 'todo/done
  )
  )

;; Buffers
;; 1
(my-leader-def
  :keymaps 'normal
  "bs" 'save-buffer
  "bk" 'kill-current-buffer
  "bj" 'kill-buffer-and-window
  "bb" 'ivy-switch-buffer
  "bf" 'find-file
  "bF" 'ido-find-file
  "bh" 'previous-buffer
  
  )

;; Windows & Navigation
;; 2
(my-leader-def
  :keymaps 'normal
  "wv" 'evil-window-vsplit
  "wk" 'hrs/split-window-below-and-switch
  "wc" 'hrs/split-window-right-and-switch
  "wo" 'find-file-other-window
  "wk" 'kill-current-buffer
  "wo" 'ace-window
  "wj" 'kill-buffer-and-window
  "wg" 'dumb-jump-go-other-window
  "wh" 'dump-jump-go
  "wb" 'dumb-jump-back
  "," 'find-file
  "." 'ivy-switch-buffer
  )

;; Search
;; 3
(my-leader-def
  :keymaps 'normal
  "ss" 'swiper
  "sS" 'swiper-all
  "sr" 'replace-string
  "seg" 'engine/search-google
  "sep" 'engine/search-duckduckgo
  "sed" 'engine/search-github
  "see" 'engine/search-prompt
  )

;; Admin
;; 4
(my-leader-def
  :keymaps 'normal
  "qq" 'save-buffers-kill-terminal
  "qv" 'emacs-version
  "qi" 'emacs-init-time
  "qu" 'emacs-uptime
  "qe" 'eshell
  )

;; Org-mode
;; 5
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
;; 6
(my-leader-def
  :keymaps 'normal
  "gg" 'magit-status
  "gi" 'magit-init
  "gm" 'git-messenger:popup-message
  "gp" 'magit-pull
  )

;; Org-roam + Content (drill)
;; 7
(my-leader-def
  :keymaps 'normal
  "nl" 'org-roam
  "ni" 'org-roam-insert
  "nf" 'org-roam-find-file
  "nc" 'org-roam-capture
  "nr" 'org-roam-random-note
  "ns" 'org-roam-server-mode
  "nd" 'org-drill
  "na" 'jk/drill
  )

;; Email / Com
;; 8
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

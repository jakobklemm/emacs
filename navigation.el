;; Navigation
(use-package counsel
  :ensure t
  :diminish ivy-mode counsel-mode
  :defines
  (projectile-completion-system magit-completing-read-function)
  :bind
  (("C-s" . swiper)
   ("M-s r" . ivy-resume)
   ("C-c v p" . ivy-push-view)
    ("C-c v o" . ivy-pop-view)
    ("C-c v ." . ivy-switch-view)
    ("M-s c" . counsel-ag)
    ("M-o f" . counsel-fzf)
    ("M-o r" . counsel-recentf)
    ("M-y" . counsel-yank-pop)
    :map ivy-minibuffer-map
    ("C-w" . ivy-backward-kill-word)
    ("C-k" . ivy-kill-line)
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)
    ("C-h" . ivy-backward-delete-char))
  :preface
    (defun ivy-format-function-pretty (cands)
      "Transform CANDS into a string for minibuffer."
      (ivy--format-function-generic
       (lambda (str)
         (concat
             (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
             (ivy--add-face str 'ivy-current-match)))
       (lambda (str)
         (concat "  " str))
       cands
       "\n"))
    :hook
    (after-init . ivy-mode)
    (ivy-mode . counsel-mode)
    :custom
    (counsel-yank-pop-height 15)
    (enable-recursive-minibuffers t)
    (ivy-use-selectable-prompt t)
    (ivy-use-virtual-buffers t)
    (ivy-on-del-error-function nil)
    (swiper-action-recenter t)
    (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
  :config
  (use-package swiper
    :ensure t
    )
  (use-package ivy
    :ensure t
    )
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)

  (setq ivy-posframe-height-alist '((swiper . 20)
				    (t      . 15)))

  (use-package amx
    :ensure t
    :after ivy
    :custom
    (amx-backend 'auto)
    (amx-save-file "~/.emacs.d/etc/amx-items")
    (amx-history-length 50)
    (amx-show-key-bindings nil)
    :config (amx-mode 1))

  (use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))

  (use-package ivy-rich
    :ensure t
    :after ivy
    :custom (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
    :config
    (ivy-rich-mode 1))

  ;; Enhance fuzzy matching
  (use-package flx
    :ensure t
    )

  (use-package ivy-posframe
    :ensure t
    :after ivy
    :init
    (ivy-posframe-mode 1)
    (setq ivy-posframe-parameters
	  '((left-fringe . 4)
	    (right-fringe . 4)))
    :config (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center))
		  ivy-posframe-border-width 4)
    :custom-face
    (ivy-posframe ((t (:background "#282a36"))))
    (ivy-posframe-border ((t (:background "#242732"))))
    (ivy-posframe-cursor ((t (:background "#95a3b0"))))
    :hook
    (ivy-mode . ivy-posframe-enable)
    )
  )

;; Ace window for easy window navigation
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  )

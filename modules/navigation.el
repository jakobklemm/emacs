;; Navigation

   (defun dw/minibuffer-backward-kill (arg)
     "When minibuffer is completing a file name delete up to parent
				 folder, otherwise delete a word"
     (interactive "p")
     (if minibuffer-completing-file-name
	   ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
	   (if (string-match-p "/." (minibuffer-contents))
	       (zap-up-to-char (- arg) ?/)
	     (delete-minibuffer-contents))
	 (backward-kill-word arg)))

   (use-package vertico
     :straight t
     :custom-face
     (vertico-current ((t (:background "#3a3f5a"))))
     :bind (:map vertico-map
		   ("C-j" . vertico-next)
		   ("C-k" . vertico-previous)
		   ("C-f" . vertico-exit)
		   :map minibuffer-local-map
		   ("C-l" . dw/minibuffer-backward-kill))
     :init
     (vertico-mode)

     ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
     (setq vertico-cycle t)
     )

   (use-package corfu
     :straight '(corfu :host github
			 :repo "minad/corfu")
     :bind (:map corfu-map
		   ("C-j" . corfu-next)
		   ("C-k" . corfu-previous)
		   ("C-f" . corfu-insert))
     :custom
     (corfu-cycle t)
     :config
     (corfu-global-mode))

   ;; Use the `orderless' completion style.
   ;; Enable `partial-completion' for files to allow path expansion.
   ;; You may prefer to use `initials' instead of `partial-completion'.
   (use-package orderless
     :straight t
     :init
     (setq completion-styles '(orderless)
	     completion-category-defaults nil
	     completion-category-overrides '((file (styles . (partial-completion))))))

   ;; Persist history over Emacs restarts. Vertico sorts by history position.
   (use-package savehist
     :straight t
     :init
     (savehist-mode))

   ;; A few more useful configurations...
   (use-package emacs
     :straight t
     :init
     ;; Add prompt indicator to `completing-read-multiple'.
     (defun crm-indicator (args)
	 (cons (concat "[CRM] " (car args)) (cdr args)))
     (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

     ;; Grow and shrink minibuffer
     ;;(setq resize-mini-windows t)

     ;; Do not allow the cursor in the minibuffer prompt
     (setq minibuffer-prompt-properties
	     '(read-only t cursor-intangible t face minibuffer-prompt))
     (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

     ;; Enable recursive minibuffers
     (setq enable-recursive-minibuffers t))
   (defun dw/get-project-root ()
     (when (fboundp 'projectile-project-root)
	 (projectile-project-root)))

   (use-package consult
     :straight t
     :demand t
     :bind (("C-s" . consult-line)
	      ("M-s" . consult-imenu)
	      :map minibuffer-local-map
	      ("C-r" . consult-history))
     :custom
     (consult-project-root-function #'dw/get-project-root)
     (completion-in-region-function #'consult-completion-in-region)
     :config
     (consult-preview-at-point-mode))

   (use-package marginalia
     :after vertico
     :straight t
     :custom
     (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
     :init
     (marginalia-mode))

   (use-package embark
     :straight t
     :bind (
	      :map minibuffer-local-map
	      ("C-d" . embark-act))
     :config

     ;; Show Embark actions via which-key
     (setq embark-action-indicator
	     (lambda (map) (which-key--show-keymap "Embark" map nil nil 'no-paging)
	       #'which-key--hide-popup-ignore-command)
	     embark-become-indicator embark-action-indicator))

    (use-package mini-frame
      :straight t
      :config
      (custom-set-variables
	'(mini-frame-show-parameters
	  '((top . 0.4)
	    (width . 0.5)
	    (left . 0.5))))
      (mini-frame-mode t)
      )

   (use-package which-key
     :straight t
     :config
     (use-package which-key-posframe
	 :straight t
	 :config
	 (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-cnter)
	 (which-key-posframe-mode t)
	 )
     (which-key-mode t)
     )

;; Ace window for easy window navigation
(use-package ace-window
  :straight t
  :init
  (setq aw-scope 'frame ; limit to single frame
        aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
  )

	(use-package bufler
	  :straight t
	  :config
	  (bufler-mode)
	  )

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (bufler-switch-buffer)
  )

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (bufler-switch-buffer)
  )

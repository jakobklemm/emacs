(use-package async
  :straight t
  )
(use-package aio
  :straight t
  )
(use-package cl-lib
  :straight t
  )
(use-package s
  :straight t
  )
(use-package dash
  :straight t
  )

(add-to-list 'load-path "~/.emacs.d/resources/")

(use-package no-littering
  :straight t
  )

(setq hrs/default-fixed-font "Fira Code")
(setq hrs/default-fixed-font-size 90)
(setq hrs/current-fixed-font-size hrs/default-fixed-font-size)
(set-face-attribute 'default nil
		    :family hrs/default-fixed-font
		    :height hrs/current-fixed-font-size)
(set-face-attribute 'fixed-pitch nil
		    :family hrs/default-fixed-font
		    :height hrs/current-fixed-font-size)

(setq hrs/font-change-increment 1.1)

(defun hrs/set-font-size ()
  "Change default, fixed-pitch, and variable-pitch font sizes to match respective variables."
  (set-face-attribute 'default nil
		      :height hrs/current-fixed-font-size)
  (set-face-attribute 'fixed-pitch nil
		      :height hrs/current-fixed-font-size)
  )

(defun hrs/reset-font-size ()
  "Revert font sizes back to defaults."
  (interactive)
  (setq hrs/current-fixed-font-size hrs/default-fixed-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font sizes by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-fixed-font-size
	(ceiling (* hrs/current-fixed-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font sizes by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-fixed-font-size
	(max 1
	     (floor (/ hrs/current-fixed-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C-=") 'hrs/increase-font-size)
(define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(hrs/reset-font-size)

(add-to-list 'custom-theme-load-path "~/.emacs.d/resources/")
(load-theme 'jeykey-dark t)

(defcustom perfect-margin-ignore-regexps
  '("^minibuf" "^[*]" "Minibuf" "[*]" "magit" "mu4e")
  "List of strings to determine if window is ignored.
Each string is used as regular expression to match the window buffer name."
  :group 'perfect-margin)

(defcustom perfect-margin-ignore-filters
  '(window-minibuffer-p)
  "List of functions to determine if window is ignored.
Each function is called with window as its sole arguemnt, returning a non-nil value indicate to ignore the window."
  :group 'perfect-margin)

(use-package perfect-margin
  :straight t
  :config
  (perfect-margin-mode 1)
  )

(use-package    feebleline
  :straight       t
  :config       (setq feebleline-msg-functions
		      '((feebleline-line-number         :post "" :fmt "%5s")
			(feebleline-column-number       :pre ":" :fmt "%-2s")
			(feebleline-file-directory      :face feebleline-dir-face :post "")
			(feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
			(feebleline-file-modified-star  :face font-lock-warning-face :post "")
			(feebleline-git-branch          :face feebleline-git-face :pre " ")
			(feebleline-project-name        :align right)
			((lambda () (format-time-string "%H:%M")) :align right)
			)
		      )
  (feebleline-mode 1))

(use-package all-the-icons
  :straight t
  )

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

(use-package ace-window
  :straight t
  :init
  (setq aw-scope 'frame ; limit to single frame
	aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
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

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(use-package bufler
  :straight t
  :config
  (bufler-mode)
  )

(use-package good-scroll
  :straight t
  :config
  (good-scroll-mode))

(setq evil-want-keybinding nil)
(setq evil-want-integration t)

(setq evil-move-beyond-eol t)
(setq evil-ex-complete-emacs-commands nil)

;; For some reason this needs to be initialized before evil...
(use-package evil-leader
  :straight t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init)
  )

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(global-set-key (kbd "C-x j") 'kill-buffer-and-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x b") 'bufler-switch-buffer)
(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(use-package format-all
  :straight t
  :bind ("C-c C-f" . format-all-buffer)
  )

(use-package magit
     :straight t
     :config
     (global-set-key (kbd "C-x g") 'magit-status)
     (global-set-key (kbd "C-x p") 'magit-init)
     (use-package magit-todos
	 :straight t
	 :config
	 (magit-todos-mode t)
	 )
     (use-package git-messenger
	 :straight t
	 )
     )

(add-to-list 'exec-path "~/.tools/elixir-ls")

(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-signature-auto-activate nil)
  :hook
  (elixir-mode . lsp)
  )

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (lsp-ui-doc-enable t)
  (lsp-ui-mode)
  (setq lsp-ui-doc-max-height 128
	     lsp-ui-doc-max-width 64
	     lsp-ui-doc-position 'top
	     lsp-ui-doc-show-with-mouse t
	     lsp-ui-doc-show-with-cursor t
	     )
  )

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.3)
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-box
  :straight t
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook (company-mode . company-box-mode)
  )

(use-package org-superstar
  :ensure t
  :config
  (setq ;;org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
   org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
   org-superstar-prettify-item-bullets t
   org-superstar-configure-like-org-bullets t
   org-hide-leading-stars nil
   org-superstar-leading-bullet ?\s
   ;; Enable custom bullets for TODO items
   org-superstar-special-todo-items t
   org-superstar-todo-bullet-alist '(("TODO" "☐ ")
				     ("NEXT" "✒ ")
				     ("STATIC" "» ")
				     ("BLOCKED" "˧ ")
				     ("DONE" "✔ ")
				     ("PAL" "✔ ")
				     )
   )
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(setq ispell-program-name "hunspell")

(setq ispell-local-dictionary "de_DE")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)
	("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "de_DE" "-a" "-i" "UTF-8") nil utf-8)))

(add-hook 'org-mode-hook #'flyspell-mode)

(add-hook 'ispell-change-dictionary-hook #'flyspell-buffer)

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
				       ("#+END_SRC" . "λ")
				       ("#+begin_src" . "λ")
				       ("#+end_src" . "λ")
				       ("#+TITLE:" . "𝙏")
				       ("#+title:" . "𝙏")
				       ("#+SUBTITLE:" . "𝙩")
				       ("#+subtitle:" . "𝙩")
				       ("#+DATE:" . "𝘿")
				       ("#+date:" . "𝘿")
				       ("#+PROPERTY:" . "☸")
				       ("#+property:" . "☸")
				       ("#+OPTIONS:" . "⌥")
				       ("#+options:" . "⌥")
				       ("#+LATEX_HEADER:" . "⇾")
				       ("#+latex_header:" . "⇾")
				       ("#+LATEX_CLASS:" . "⇥")
				       ("#+latexx_class:" . "⇥")
				       ("#+ATTR_LATEX:" . "🄛")
				       ("#+attr_latex:" . "🄛")
				       ("#+LATEX:" . "ℓ")
				       ("#+latex:" . "ℓ")
				       ("#+ATTR_HTML:" . "🄗")
				       ("#+attr_html:" . "🄗")
				       ("#+BEGIN_QUOTE:" . "❮")
				       ("#+begin_quote:" . "❮")
				       ("#+END_QUOTE:" . "❯")
				       ("#+end_quote:" . "❯")
				       ("#+CAPTION:" . "☰")
				       ("#+caption:" . "☰")
				       (":PROPERTIES:" . "⚙")
				       (":properties:" . "⚙")
				       ("#+AUTHOR:" . "A")
				       ("#+author:" . "A")
				       ("#+IMAGE:" . "I")
				       ("#+image:" . "I")
				       ("#+LANGUAGE:" . "L")
				       ("#+language:" . "L")
				       ))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(use-package org-roam
  :straight t
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (setq
   org-roam-directory (file-truename "~/documents/vaults/database/")
   org-roam-db-location "~/documents/vaults/org-roam.db"
   org-roam-db-gc-threshold most-positive-fixnum
   )
  :config
  (setq org-roam-capture-templates
	'(("d" "default" plain (function org-roam--capture-get-point)
	   "%?"
	   :file-name "${slug}"
	   :head "#+TITLE: ${title}\n"
	   :immediate-finish t
	   :unnarrowed t)
	  ))
  (use-package org-roam-server
    :ensure t
    :config
    (setq org-roam-server-host "127.0.0.1"
	  org-roam-server-port 8080
	  org-roam-server-authenticate nil
	  org-roam-server-export-inline-images t
	  org-roam-server-serve-files nil
	  org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "jpg" "png")
	  org-roam-server-network-poll t
	  org-roam-server-network-arrows nil
	  org-roam-server-network-label-wrap-length 20))
  )

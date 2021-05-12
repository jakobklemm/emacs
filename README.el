(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
      '(
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(straight-use-package 'use-package)

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

(add-to-list 'load-path "~/.emacs.d/reesources/")

;; Treat CamelCaseSubWords as separate words in every programming
;; mode.
(add-hook 'prog-mode-hook 'subword-mode)

;; Don't assume that sentences should have two spaces after
;; periods.
(setq sentence-end-double-space nil)

;; Turn on transient-mark-mode.
(transient-mark-mode t)

;; Auto wrap text
(auto-fill-mode t)

;; selected text and start inserting your typed text.
(delete-selection-mode t)

;; If you save a file that doesn't end with a newline, automatically
;; append one.
(setq require-final-newline t)

;; Visually indicate matching pairs of parentheses.
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; When you perform a problematic operation, flash the screen instead
;; of ringing the terminal bell.
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Don't ask `yes/no?', ask `y/n?'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Don't present the usual startup message, and clear the scratch buffer.
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; When something changes a file, automatically refresh the buffer
;; containing that file so they can't get out of sync.
(global-auto-revert-mode t)

;; Move everything to trash first
(setq delete-by-moving-to-trash t)

;; No reason to use any other type. Might be disabled dependant on the
;; current mode.
(setq display-line-numbers-type 'relative)

;; Use tabs for everything (https://youtu.be/SsoOG6ZeyUI)
(setq indent-tabs-mode t)
(setq indent-line-function 'insert-tab)

;; Launch emacs in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Save the location within a file.
(save-place-mode t)

;; Set always to UTF-8
(set-language-environment "UTF-8")

;; Menu bar
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;; Minibuffer
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Use smoth scrolling
(setq scroll-conservatively 100)

;; Highlight the current line
(global-hl-line-mode)

;; Hide the modeline
(setq mode-line-format nil)

;; Make it affect all buffers.
(setq-default mode-line-format nil)

;; Line wrap mode
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'gfm-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Middle click to paste from clipbord.
(setq mouse-yank-at-point t)

(desktop-save-mode 1)

;; Delete trailing whitespaces & insert final newline.
(add-hook 'before-save-hook 'unix-newline)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq byte-compile-warnings '(cl-functions))

(use-package no-littering
  :straight t
  )

(add-to-list 'custom-theme-load-path "~/.emacs.d/resources/")
(load-theme 'jeykey-dark t)

(set-cursor-color "#EF7E8D")

(set-frame-font "Hasklig 11" nil t)

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
  (feebleline-mode 1)
  )

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package all-the-icons
  :straight t
  )

(declare-function all-the-icons-faicon 'all-the-icons)
(declare-function all-the-icons-material 'all-the-icons)
(declare-function all-the-icons-octicon 'all-the-icons)
(setq company-box-icons-all-the-icons
      `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
	(Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
	(Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
	(Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
	(Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
	(Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
	(Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
	(Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
	(Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
	(Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
	(Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
	(Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
	(Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
	(Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
	(Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
	(Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
	(Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
	(File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
	(Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
	(Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
	(EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
	(Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
	(Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
	(Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
	(Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
	(TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
	(Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
      company-box-icons-alist 'company-box-icons-all-the-icons)

(use-package hl-todo
  :straight t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
	 (yaml-mode . hl-todo-mode))
  )

(use-package rainbow-mode
  :straight t
  :hook '(prog-mode help-mode)
  )

(use-package helm
  :straight t
  :config

  (require 'helm-config)

  (setq helm-M-x-always-save-history t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)
  (define-key helm-map (kbd "C-l") 'helm-dfind-files-up-one-level)

  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  )

(use-package helm-posframe
  :straight t
  :init
  (setq helm-posframe-width 200)
  :config
  (helm-posframe-enable)
  )

(use-package recentf
  :straight t
  :custom
  (recentf-auto-cleanup 30)
  :config
  (recentf-mode)
  (run-with-idle-timer 10 t 'recentf-save-list)
  )

(use-package mwheel
  :custom
  (mouse-wheel-scroll-amount '(1
			       ((shift) . 5)
			       ((control))))
  (mouse-wheel-progressive-speed nil))

(use-package pixel-scroll
  :config
  (pixel-scroll-mode))

;; Credit: https://github.com/hrs
(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (helm-mini)
  )

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (helm-mini)
  )

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  )

(use-package which-key-posframe
  :straight t
  :config
  (which-key-posframe-mode)
  )

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
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

(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

(use-package dtrt-indent
  :straight t
  :config
  (dtrt-indent-global-mode)
  (dtrt-indent-adapt))

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

(setq tab-width 4)
(setq evil-shift-width 4)

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

(use-package smartparens
  :straight t
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  )

(use-package popup-kill-ring
  :straight t
  :bind ("M-y" . popup-kill-ring)
  )

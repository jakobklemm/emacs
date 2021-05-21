(setq package-enable-at-startup nil)

(setq gc-cons-threshold 1000000000)

(menu-bar-mode -1)

(set-window-scroll-bars (minibuffer-window) nil nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; More defaults
;; Treat CamelCaseSubWords as separate words in every programming
;; mode.
(global-subword-mode)

;; Don't assume that sentences should have two spaces after
;; periods.
(setq sentence-end-double-space nil)

;; Turn on transient-mark-mode.
(transient-mark-mode t)

;; Disable native comp warnings.
(setq comp-async-report-warnings-errors nil)

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

(provide 'early-init)

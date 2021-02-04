;; Defaults and setup
;; Partial credit to https://github.com/hrs/sensible-defaults.el

;; Allow 20MB of memory (instead of 0.76MB) before calling garbage
;; collection. This means GC runs less often, which speeds up some
;; operations.
(setq gc-cons-threshold 20000000)

;; Call DELETE-TRAILING-WHITESPACE every time a buffer is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat CamelCaseSubWords as separate words in every programming
;; mode.
(add-hook 'prog-mode-hook 'subword-mode)

;; Don't assume that sentences should have two spaces after
;; periods. This ain't a typewriter.
(setq sentence-end-double-space nil)

;; Turn on transient-mark-mode.
(transient-mark-mode t)

;; Auto wrap text
(auto-fill-mode t)

;; If some text is selected, and you type some text, delete the

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

;; Set always to UTF-8, only display in bar if not UTF-8
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
;; (setq mode-line-format nil)

;; Error handling
(setq warning-minimum-level :emergency)

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

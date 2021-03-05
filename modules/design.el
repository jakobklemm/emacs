;; Design / look & feel of emacs

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes

  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
	custom-safe-themes t
	)

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f4>" . modus-themes-toggle))

(global-prettify-symbols-mode 1)

(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("->" . 8594)
          ("<-" . 8592)
          ("<=" . 8804)
          (">=" . 8805)
          ("=~" . 8771)
          ("!=" . 8800)
          )
        )
  )

(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

(set-frame-font "Iosevka 11" nil t)

(set-cursor-color "#D069D6")

(use-package beacon
  :ensure t
  :disabled t
  :custom
  (beacon-color "#D271D8")
  :hook (after-init . beacon-mode))

;; Margins
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
  :ensure t
  :config
  (perfect-margin-mode 1)
  )

(defun current-buffer ()
  (interactive)
  (message (buffer-name))
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package all-the-icons
  :ensure t
  )

(add-hook 'prog-mode-hook #'hl-todo-mode)

(use-package mini-modeline
  :ensure t
  :init
  (setq
   mini-modeline-display-gui-line nil
   mini-modeline-frame nil
   mini-modeline-truncate-p t
   mini-modeline-enhance-visual nil
   mini-modeline-r-format (setq mode-line-format
				(list "-"
				      'mode-line-mule-info
				      'mode-line-modified
				      ;; Note that this is evaluated while making the list.
				      ":"
				      'default-directory
				      ":"
				      'mode-name
				      ":"
				      '(line-number-mode "L%l")
				      ))
   )
  :config
  (mini-modeline-mode t))

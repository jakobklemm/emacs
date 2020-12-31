;; Design / look & feel of emacs

(use-package mood-one-theme
  :ensure t
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'mood-one)
  )

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
  :custom
  (beacon-color "#D271D8")
  :hook (after-init . beacon-mode))

;; Margins
(defcustom perfect-margin-ignore-regexps
  '("^minibuf" "^[*]" "Minibuf" "[*]" "magit")
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

;;(setq-default left-margin-width 24 right-margin-width 48)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package all-the-icons
  :ensure t
  )

;; Darkroom mode (https://www.draketo.de/light/english/simple-emacs-darkroom) (not the "offical" package, but just a function)
; simple darkroom with fullscreen, fringe, mode-line, menu-bar and scroll-bar hiding.
(defvar darkroom-enabled nil)
(defvar darkroom-menu-bar-enabled nil)

(defun toggle-darkroom ()
  (interactive)
  (if (not darkroom-enabled)
      (setq darkroom-enabled t)
    (setq darkroom-enabled nil))
  (if darkroom-enabled
      (progn
        ; if the menu bar was enabled, reenable it when disabling darkroom
        (if menu-bar-mode
            (setq darkroom-menu-bar-enabled t)
          (setq darkroom-menu-bar-enabled nil))
        ; save the frame configuration to be able to restore to the exact previous state.
        (if darkroom-menu-bar-enabled
            (menu-bar-mode -1))
        (scroll-bar-mode -1)
        (let ((fringe-width
               (* (window-width (get-largest-window))
                  (/ (- 1 0.61803) (1+ (count-windows)))))
              (char-width-pixels 6))
        ; 8 pixels is the default, 6 is the average char width in pixels
        ; for some fonts:
        ; http://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
           (set-fringe-mode (truncate (* fringe-width char-width-pixels))))

        (add-hook 'after-save-hook 'count-words-and-characters-buffer))

    (progn
      (if darkroom-menu-bar-enabled
          (menu-bar-mode))
      (set-fringe-mode nil)
      (remove-hook 'after-save-hook 'count-words-and-characters-buffer)
      )))

(global-set-key [f11] 'toggle-darkroom)

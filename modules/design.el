;; Design / look & feel of emacs

(load-theme 'jeykey-dark t)

(global-prettify-symbols-mode 1)

 (setq hrs/default-fixed-font "Iosevka")
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

(set-cursor-color "#D069D6")

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
  :straight t
  :config
  (perfect-margin-mode 1)
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

(add-hook 'prog-mode-hook #'hl-todo-mode)

(use-package    feebleline
  :straight t
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

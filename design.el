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

;;(set-frame-font "JetBrains Mono 11" nil t)
(set-frame-font "Iosevka 11" nil t)

(set-cursor-color "#D069D6")

(use-package beacon
  :ensure t
  :custom
  (beacon-color "#D271D8")
  :hook (after-init . beacon-mode))

(use-package all-the-icons
  :ensure t
  )

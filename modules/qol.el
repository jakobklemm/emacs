;; Quality of life changes

;; Install no-littering to handle all temporary and backup files.
(use-package no-littering
  :ensure t
  )

;; Better window switch functionality
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; For highly serious work
(use-package malyon
  :ensure t
  )

(use-package async
  :ensure t
  )
(use-package aio
  :ensure t
  )
(use-package cl-lib
  :ensure t
  )
(use-package s
  :ensure t
  )
(use-package dash
  :ensure t
  )

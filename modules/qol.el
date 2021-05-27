;; Quality of life changes

;; Install no-littering to handle all temporary and backup files.
(use-package no-littering
  :straight t
  )

;; Better window switch functionality
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; For highly serious work
(use-package malyon
  :straight t
  )

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

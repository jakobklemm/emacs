;; Quality of life changes

;; Install no-littering to handle all temporary and backup files.
(use-package no-littering
  :ensure t
  )

;; Better window switch functionality
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Credit: https://github.com/hrs
(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  ;;(ivy-switch-buffer)
  (helm-mini)
  )

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  ;;(ivy-switch-buffer)
  (helm-mini)
  )

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; For highly serious work
(use-package malyon
  :ensure t
  )

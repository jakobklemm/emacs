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
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;; Keys
(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

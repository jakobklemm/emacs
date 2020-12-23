;; Org-mode / looks & feel

(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
	;;org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
	org-superstar-prettify-item-bullets t
	;; Enable custom bullets for TODO items
	org-superstar-special-todo-items t
	org-superstar-todo-bullet-alist '(("TODO" "☐ ")
					  ("NEXT" "✒ ")
					  ("STATIC" "» ")
					  ("BLOCKED" "˧ ")
					  ("DONE" "✔ ")
					  ("PAL" "✔ ")
					  )
	)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

;; General settings & priority colors
(setq org-ellipsis " ▼ "
      org-adapt-indentation nil
      org-fontify-quote-and-verse-blocks t
      org-startup-folded t
      org-priority-highest ?A
      org-priority-lowest ?C
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow))
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t
      org-src-window-setup 'current-window
      org-return-follows-link t
      org-confirm-babel-evaluate nil
      org-use-speed-commands t
      )

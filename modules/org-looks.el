;; Org-mode / looks & feel

(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
	;;org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
	org-superstar-prettify-item-bullets t
	org-superstar-configure-like-org-bullets t
	org-hide-leading-stars nil
	org-superstar-leading-bullet ?\s
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
      org-catch-invisible-edits 'show

      ;; Inline images
      org-startup-with-inline-images t
      org-image-actual-width '(600)
      )

(require 'org-pretty-table)
(add-hook 'org-mode-hook 'org-pretty-table-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
;; Deadline colors
(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

;; Heading sizes
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.60))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.40))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.20))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Special names / items
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "λ")
                                       ("#+END_SRC" . "λ")
                                       ("#+begin_src" . "λ")
                                       ("#+end_src" . "λ")
				       ("#+TITLE:" . "𝙏")
				       ("#+title:" . "𝙏")
				       ("#+SUBTITLE:" . "𝙩")
				       ("#+subtitle:" . "𝙩")
				       ("#+DATE:" . "𝘿")
				       ("#+date:" . "𝘿")
				       ("#+PROPERTY:" . "☸")
				       ("#+property:" . "☸")
				       ("#+OPTIONS:" . "⌥")
				       ("#+options:" . "⌥")
				       ("#+LATEX_HEADER:" . "⇾")
				       ("#+latex_header:" . "⇾")
				       ("#+LATEX_CLASS:" . "⇥")
				       ("#+latexx_class:" . "⇥")
				       ("#+ATTR_LATEX:" . "🄛")
				       ("#+attr_latex:" . "🄛")
				       ("#+LATEX:" . "ɫ")
				       ("#+latex:" . "ɫ")
				       ("#+ATTR_HTML:" . "🄗")
				       ("#+attr_html:" . "🄗")
				       ("#+BEGIN_QUOTE:" . "❮")
				       ("#+begin_quote:" . "❮")
				       ("#+END_QUOTE:" . "❯")
				       ("#+end_quote:" . "❯")
				       ("#+CAPTION:" . "☰")
				       ("#+caption:" . "☰")
				       (":PROPERTIES:" . "⚙")
				       (":properties:" . "⚙")
				       ("#+AUTHOR:" . "A")
				       ("#+author:" . "A")
				       ))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

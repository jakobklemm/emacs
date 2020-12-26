;; Org-mode latex (export & editing)

(eval-after-load "org" '(require 'ox-odt nil t))

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode)
  )

(use-package ob-go
  :ensure t
  )
(use-package ob-elixir
  :ensure t
  )

(use-package htmlize
  :ensure t)

(use-package ox-reveal
  :ensure t
  :custom ((org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
	   (org-reveal-mathjax t)
	   (org-reveal-ignore-speaker-notes nil)
	   (org-reveal-note-key-char nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (python . t)
   (go . t)
   (sql . t)
   (elixir . t)
   ))

(setq org-confirm-babel-evaluate nil)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-math-mode)
	    (setq TeX-master t)))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

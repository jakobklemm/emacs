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

(use-package ox-pandoc
  :ensure t
  )

(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.tools/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  )

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
   (plantuml . t)
   ))

(setq org-confirm-babel-evaluate nil)

(setq TeX-parse-self t)
(setq TeX-auto-save t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (LaTeX-math-mode)
	    (setq TeX-master t)))

(use-package bibtex-completion
  :ensure t
  :config
  (setq bibtex-completion-notes-path "~/documents/supervisor"
        bibtex-completion-bibliography "~/.tools/references.bib"
	)
  )

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Custom title page & templating using inserts
(defun jk/title-title ()
  (car (org-roam--extract-titles-title))
  )

(defun jk/title-author ()
  (cdr (car (org-roam--extract-global-props '("AUTHOR"))))
  )
(defun jk/title-image ()
  (cdr (car (org-roam--extract-global-props '("IMAGE"))))
  )
(defun jk/title-subtitle ()
  (cdr (car (org-roam--extract-global-props '("SUBTITLE"))))
  )

(defun jk/title-compose ()
  (interactive)
  (insert (concat "
#+LATEX_HEADER: \\usepackage[utf8]{inputenc}
#+LATEX_HEADER: \\usepackage[dvipsnames]{xcolor}
#+LATEX_HEADER: \\usepackage{tikz}
#+LATEX_HEADER: \\usepackage[]{babel}
\\begin{titlepage}
    \\begin{center}
        \\begin{tikzpicture}[remember picture,overlay]
            \\node[anchor=north west,yshift=-1.5pt,xshift=1pt]%
            at (current page.north west)
            {\\includegraphics[scale=1]{~/.tools/"
		  (jk/title-image)
		  ".png}};
\\end{tikzpicture}

        \\vspace{2.2cm}

        \\Huge
        \\textbf{"
		  (jk/title-title)
		  "}

        \\vspace{3.0cm}
        \\LARGE"
		  (jk/title-subtitle)
		  "
\\vspace{4.2cm}"

		  (jk/title-author)

	"\\
        \\vfill

        \\Large
        Baden, Schweiz\\
        \\today
    \\end{center}
\\end{titlepage}
\\tableofcontents
\\newpage"
	   )
	  )
  )

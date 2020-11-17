;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! lsp-mode)
(package! lsp-ui)
(package! lsp-ivy)
(package! company-box)
(package! company-lsp)

(package! helm)
(package! helm-swoop)
(package! dumb-jump)
(package! darkroom)

(package! toc-org)
(package! org-superstar)
(package! org-super-agenda)
(package! org-fragtog)
(package! org-roam)
(package! org-roam-server)
(package! org-graph-view :recipe (:host github :repo "alphapapa/org-graph-view"))
(package! org-pretty-table-mode
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-ref)
(package! org-ql)

;; Media
(package! 2048-game)
(package! speed-type)
(package! malyon)

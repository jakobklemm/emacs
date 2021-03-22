;; Org-mode tools & extras

(add-hook 'org-mode-hook 'org-toggle-inline-images)
(setq org-image-actual-width '(600))


;; Default apps
(setq org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))

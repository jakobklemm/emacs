;;; wikijs.el -*- lexical-binding: t; -*-

(defun convert-and-save ()
  (interactive)
  (org-pandoc-export-as-markdown)
  (switch-to-buffer "*Pandoc*")
  (write-region (point-min) (point-max) (concat "/home/jeykey/documents/projects/wiki/database/" (car (plist-get (org-export-get-environment) ':title)) ".md"))
  (kill-buffer-and-window)
  )

(defun get-the-buffer ()
  (interactive)
  (message (current-buffer))
  )

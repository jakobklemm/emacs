;; Author: Jakob Klemm
;; Version: 1.0.0

(require 'org)

(org-link-set-parameters
 "md5"
 :follow (lambda (path) (shell-command (concat "echo " path)))
 :export (lambda (path desc backend)
           (interactive "FFind file: ")
           (shell-command-to-string (concat "echo " (buffer-file-name (find-file-noselect filename)))))
 :face '(:foreground "yellow")
 :help-echo "Rodian cloud file system with md5 indexing.")

(defun md5-find (filename)
  "md5 stuff"
  (interactive "FFind file: ")
  (shell-command-to-string (concat "echo " (buffer-file-name (find-file-noselect filename)))))

(provide 'rodian)

;; Author: Jakob Klemm
;; Version: 1.0.0

(require 'org)

(org-link-set-parameters
 "md5"
 :follow (lambda (path) (md5-open path))
 :export #'md5-store
 :face '(:foreground "yellow")
 :help-echo "Rodian cloud file system with md5 indexing.")

(defun md5-find (filename)
  "md5 stuff"
  (interactive "FFind file: ")
  (shell-command (concat "echo " (buffer-file-name (find-file-noselect filename))))
  )

(defun md5-open (file)
  (shell-command (concat "echo " file))
  )

(defun md5-store ()
  "Store a link to a man page."
  (let* (page (get-file ()))
    (link (concat "md5:" page))
    (org-link-store-props
     :type "md5"
     :link link
     :description description)))

(defun get-file (filename)
  "md5 stuff"
  (interactive "FFind file: ")
  (shell-command (concat "echo " (buffer-file-name (find-file-noselect filename))))
  )

(defun insert-md5 (filename)
  (interactive "FFind file: ")
  (insert (buffer-file-name (find-file-noselect filename)))
  )

(provide 'rodian)
;;; rodian.el ends here

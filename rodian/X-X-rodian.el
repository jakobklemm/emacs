;; Author: Jakob Klemm
;; Version: 1.0.0

(require 'org)

(org-link-set-parameters
 "red"
 :follow (lambda (path) (message "You clicked me."))
 :export (lambda (path desc backend)
           (cond
            ((eq 'html backend)
             (format "<font color=\"red\">%s</font>"
                     (or desc path)))))
 :face '(:foreground "red")
 :help-echo "Click me for a message.")

(defun md5-find (filename)
  "md5 stuff"
  (interactive "FFind file: ")
  (shell-command (concat "echo " (buffer-name (find-file filename)))))

(provide 'rodian)

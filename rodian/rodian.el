;; Author: Jakob Klemm
;; Version: 1.0.0

;;; Code:

(defun md5-find ()
  "Md5 stuff."
  (interactive)
  ;; (shell-command (concat "echo " (buffer-file-name (find-file-noselect filename))))
  (read-file-name "Enter file name:")
  )

(defun get-desc ()
  "Stuff."
  (interactive)
  (read-string "File: ")
  )

(defun insert-md5 (type filename desc)
  "Insert selected file, FILENAME DESC TYPE ."
  (insert "[[" (shell-command-to-string (concat "hoth " type filename)) "][" desc "]]")
  )

(defun type (choice)
  "Interactive select, CHOICE."
  (interactive)
   (let ((completion-ignore-case  t))
     (list (completing-read "File type: " '("MD5" "UUID") nil t)))
  )

(defun md5-total ()
  "File."
  (interactive)
  (insert-md5 (car(type ())) (md5-find) (get-desc))
  )

(provide 'rodian)
;;; rodian.el ends here

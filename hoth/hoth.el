;;; package -- Hoth storage system.
;; Author: Jakob Klemm
;; Version: 1.0.0

;;; Code:

(defun hoth-find ()
  "Select a file in interactive mode and return the filepath."
  (interactive)
  (read-file-name "Enter file name:")
  )

(defun string-trim-newline (string)
  "Remove the \n from bash output as STRING."
  (let ((len (length string)))
    (cond
      ((and (> len 0) (eql (aref string (- len 1)) ?\n))
       (substring string 0 (- len 1)))
      (t string))))

(defun hoth-insert (type filename)
  "Write the generated link to the current file with the hoth shell script, TYPE = md5 or uuid, FILENAME = absolute file path."
  (insert "[[" (string-trim-newline (shell-command-to-string (concat "~/.doom.d/hoth/hoth.sh " type " \"" filename "\""))) "][" (file-name-nondirectory filename) "]]")
  )

(defun hoth-type (choice)
  "Select one of the two available modes as CHOICE."
  (interactive)
   (let ((completion-ignore-case  t))
     (list (completing-read "File type: " '("md5" "uuid") nil t)))
  )

(defun hoth-total ()
  "The main call function for the hoth storage system."
  (interactive)
  (hoth-insert (car(hoth-type ())) (hoth-find))
  )

(provide 'hoth)
;;; hoth.el ends here

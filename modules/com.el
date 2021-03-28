;; Communication

;; default
(setq mu4e-maildir (expand-file-name "~/.mail"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)
;; (add-hook 'mu4e-view-mode-hook 'normal-mode)

(setq mu4e-completing-read-function 'ivy-completing-read)
(setq mail-user-agent 'mu4e-user-agent)

(setq user-mail-address "jakob@jeykey.net"
      user-full-name  "Jakob Klemm"

      mu4e-get-mail-command "mbsync -c ~/.tools/.mbsyncrc -a"
      mu4e-update-interval  300
      mu4e-index-update-in-background t
      mu4e-main-buffer-hide-personal-addresses t

      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-smtpmail-send-it
      starttls-use-gnutls t

      mu4e-sent-messages-behavior 'delete

      mu4e-view-show-addresses t

      message-kill-buffer-on-exit t

      mu4e-attachment-dir  "~/documents/vaults/ram"

      mu4e-sent-folder "/global/Sent"
      mu4e-drafts-folder "/global/Drafts"
      mu4e-trash-folder "/global/Trash"
      message-signature
      (concat
       "Jakob Klemm\n"
       "https://github.com/jakobklemm"
       "https://jeykey.net\n")
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t
      mml-secure-smime-sign-with-sender "jakob@jeykey.net"

      mu4e-view-prefer-html t

      )

(load-file "~/.tools/mail.el")

(setq smtpmail-starttls-credentilas my-mu4e-account-alist)
(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)


(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  )

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; https://github.com/munen/emacs.d/#mu4e
(defun ok/message-attachment-present-p ()
  "Return t if a non-gpg attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part type" nil t) t))))

(setq ok/message-attachment-regexp
  (regexp-opt '("[Ww]e send"
                "[Ii] send"
                "attach"
                "[aA]ngehängt"
                "[aA]nhang"
                "[sS]chicke"
                "angehaengt"
                "haenge"
                "hänge")))

(defun ok/message-warn-if-no-attachments ()
  "Check if there is an attachment in the message if I claim it."
  (when (and (save-excursion
         (save-restriction
     (widen)
     (goto-char (point-min))
     (re-search-forward ok/message-attachment-regexp nil t)))
       (not (ok/message-attachment-present-p)))
    (unless (y-or-n-p "No attachment. Send the message?")
      (keyboard-quit))))

(add-hook 'message-send-hook #'ok/message-warn-if-no-attachments)
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)

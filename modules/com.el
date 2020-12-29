;; Communication
(setq user-mail-address "jakob@jeykey.net"
      user-full-name  "Jakob Klemm"
      ;; I have my mbsyncrc in a different folder on my system, to keep it separate from the
      ;; mbsyncrc available publicly in my dotfiles. You MUST edit the following line.
      ;; Be sure that the following command is: "mbsync -c ~/.config/mu4e/mbsyncrc -a"
      ;; mu4e-get-mail-command "mbsync -c ~/.config/mu4e-dt/mbsyncrc -a"
      mu4e-update-interval  300
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
      mu4e-sent-folder "/account-1/Sent"
      mu4e-drafts-folder "/account-1/Drafts"
      mu4e-trash-folder "/account-1/Trash"
      mu4e-maildir-shortcuts
      '(("/account-1/Inbox"      . ?i)
        ("/account-1/Sent Items" . ?s)
        ("/account-1/Drafts"     . ?d)
        ("/account-1/Trash"      . ?t))
      message-signature
      (concat
       "Jakob Klemm\n"
       "jakob@jeykey.net\n"
       "https://jeykey.net\n")
      mml-secure-openpgp-sign-with-sender t
      mml-secure-smime-sign-with-sender "Jakob Klemm (Laptop)"
      )

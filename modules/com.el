;; Communication
;;(require 'mu4e)

;; default
(setq mu4e-maildir (expand-file-name "~/.mail"))

(setq user-mail-address "jakob@jeykey.net"
      user-full-name  "Jakob Klemm"
      ;; I have my mbsyncrc in a different folder on my system, to keep it separate from the
      ;; mbsyncrc available publicly in my dotfiles. You MUST edit the following line.
      ;; Be sure that the following command is: "mbsync -c ~/.config/mu4e/mbsyncrc -a"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval  300
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("mail.cyon.ch" 587 nil nil))
      mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-maildir-shortcuts
      '(("/Inbox"      . ?i)
        ("/Sent Items" . ?s)
        ("/Drafts"     . ?d)
        ("/Trash"      . ?t))
      message-signature
      (concat
       "Jakob Klemm\n"
       "https://github.com/jakobklemm"
       "https://jeykey.net\n")
      mml-secure-openpgp-sign-with-sender t
      mml-secure-smime-sign-with-sender "jakob@jeykey.net"
      )

;;; which-key-posframe-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "which-key-posframe" "which-key-posframe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from which-key-posframe.el

(autoload 'which-key-posframe-enable "which-key-posframe" "\
Enable which-key-posframe." t nil)

(defvar which-key-posframe-mode nil "\
Non-nil if Which-Key-Posframe mode is enabled.
See the `which-key-posframe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-key-posframe-mode'.")

(custom-autoload 'which-key-posframe-mode "which-key-posframe" nil)

(autoload 'which-key-posframe-mode "which-key-posframe" "\
Toggle which key posframe mode on of off.

If called interactively, toggle `Which-Key-Posframe mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "which-key-posframe" '("which-key-"))

;;;***

(provide 'which-key-posframe-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-posframe-autoloads.el ends here

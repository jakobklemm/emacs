;;; feebleline-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "feebleline" "feebleline.el" (0 0 0 0))
;;; Generated autoloads from feebleline.el

(defvar feebleline-mode nil "\
Non-nil if Feebleline mode is enabled.
See the `feebleline-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `feebleline-mode'.")

(custom-autoload 'feebleline-mode "feebleline" nil)

(autoload 'feebleline-mode "feebleline" "\
Replace modeline with a slimmer proxy.

If called interactively, toggle `Feebleline mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "feebleline" '("feebleline-"))

;;;***

(provide 'feebleline-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; feebleline-autoloads.el ends here

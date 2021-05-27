;;; perfect-margin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "perfect-margin" "perfect-margin.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from perfect-margin.el

(defvar perfect-margin-mode nil "\
Non-nil if Perfect-Margin mode is enabled.
See the `perfect-margin-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `perfect-margin-mode'.")

(custom-autoload 'perfect-margin-mode "perfect-margin" nil)

(autoload 'perfect-margin-mode "perfect-margin" "\
Auto center windows.

If called interactively, enable Perfect-Margin mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "perfect-margin" '("perfect-margin-")))

;;;***

(provide 'perfect-margin-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; perfect-margin-autoloads.el ends here

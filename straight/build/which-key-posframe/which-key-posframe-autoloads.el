;;; which-key-posframe-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Which-Key-Posframe mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "which-key-posframe" '("which-key-")))

;;;***

(provide 'which-key-posframe-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; which-key-posframe-autoloads.el ends here

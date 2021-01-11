;; EXWM & Window / OS Management

;; Ivy-posframe doesn't work with most EXWM windows. Instead use ido for all exwm tasks,
;; since it can fit in the one line high bar at the bottom of each window.
;; Ido binds are the same as ivy, but prefixed with super instead of space.

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  )

(use-package exwm
  :ensure t
  :disabled t
  :config
  (require 'exwm-config)
  (require 'exwm-randr)


  (exwm-config-default)

  (setq exwm-randr-workspace-output-plist '(0 "DP-2" 1 "DP-0"))

  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command "xrandr" nil "xrandr --output DVI-D-0 --off --output HDMI-0 --off --output HDMI-1 --off --output DP-0 --mode 1920x1080 --pos 2560x180 --rotate normal --output DP-1 --off --output DP-2 --primary --mode 2560x1440 --rate 144.00 --pos 0x0 --rotate normal --output DP-3 --off"
					   )
	      )
	    )

  (exwm-randr-enable)

  (setq exwm-workspace-number 10)

  (exwm-input-set-simulation-keys nil)

  (setq exwm-input-prefix-keys
	'(?\s-\
	  ?\s
	  ?\C-M
	  ?\C-b
	  )
	)

  (exwm-enable)

  )

(add-to-list 'display-buffer-alist
  (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(setq package-enable-at-startup nil)

(setq gc-cons-threshold 1000000000)

(menu-bar-mode -1)

(set-window-scroll-bars (minibuffer-window) nil nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'early-init)

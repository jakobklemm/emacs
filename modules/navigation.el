;; Navigation

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source  t
	helm-ff-file-name-history-use-recentf t
	helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t
	)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-c") 'helm-calcul-expression)
  (global-set-key (kbd "C-s") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-l") 'helm-find-files-up-one-level)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  )

(setq helm-posframe-width 200)

(require 'helm-posframe)
(helm-posframe-enable)

;; Ace window for easy window navigation
(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame ; limit to single frame
        aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n))
  )

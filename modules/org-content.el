;;  Org-mode content

(use-package org-roam
  :ensure t
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (setq
   org-roam-directory (file-truename "~/documents/vaults/database/")
   org-roam-db-location "~/documents/vaults/org-roam.db"
   org-roam-graph-exclude-matcher "supervisor"
   org-roam-db-gc-threshold most-positive-fixnum
   org-roam-graph-exclude-matcher "private"
   org-roam-tag-sources '(prop last-directory)
   )
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("f" "unchanged" plain (function org-roam-capture--get-point)
           "%?"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)))
  (use-package org-roam-server
    :ensure t
    :config
    (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv" "jpg" "png")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))
  )

(use-package files
  :custom
  (backup-directory-alist `(("." . "~/.saves")))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

;; put emacs custom-set-variables to separate file
(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/emacs-custom.el")
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file))

(provide 'm-backups)

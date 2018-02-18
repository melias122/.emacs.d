;; put emacs custom-set-variables to separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")

;; fix error when no file exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

(provide 'm-custom)

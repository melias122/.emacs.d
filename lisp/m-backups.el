(setq backup-directory-alist `(("." . "~/.saves"))
			backup-by-copying t
			delete-old-versions t
			kept-new-versions 6
			kept-old-versions 2
			version-control t)

(provide 'm-backups)

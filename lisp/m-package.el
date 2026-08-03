;; Packages are activated automatically before init; only add archives here.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap use-package for older emacs versions
(if (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-when-compile
	(require 'use-package))

;; Required by use-package
(use-package diminish :ensure t)
(use-package delight  :ensure t)

(provide 'm-package)

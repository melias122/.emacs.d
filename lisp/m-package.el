;; Initialize package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set before loading use-package
(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
	(require 'use-package))

;; Required by use-package
(use-package diminish)
(use-package bind-key)
(use-package delight)

(provide 'm-package)

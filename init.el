;; m's custom emacs configuration.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
	(require 'use-package))

(when (memq window-system '(mac ns x))
  (set-default-font "Monaco 15"))


(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (require 'm-custom)
  (require 'm-backups)
  (require 'm-frame)
  (require 'm-keybindings)
  (require 'm-themes)
  (require 'm-fonts)
  (require 'm-ivy)
  (require 'm-project)
  (require 'm-json)
  (require 'm-stuff)
  (require 'm-programming))

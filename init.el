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

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package highlight-parentheses
  :ensure t
  :init (global-highlight-parentheses-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(show-paren-mode 1)

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
  (require 'm-go))

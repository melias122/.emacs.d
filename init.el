;;
;; Martin Elias custom emacs configuration.
;;
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
(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac x ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (require 'm-backups)
  (require 'm-frame)
  (require 'm-functions)
  (require 'm-ivy)
  (require 'm-programming))

;; package to try (suggested by Fionn)
;;
;; expand-region
;; iedit
;; visual-regexp
;; avy
;; hydra
;; find-file-in-project (only really need "find-file-in-current-directory", very useful outside repos)
;; ws-butler
;; wgrep
;; all

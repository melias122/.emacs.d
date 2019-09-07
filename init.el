;;
;; m's custom emacs configuration.
;;

;; Initialize package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
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

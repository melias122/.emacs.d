;; m's custom emacs configuration.
;;

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; lsp recommends
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp/editorconfig")
  (require 'm-backups)
  (require 'm-frame)
  (require 'm-functions)
  (require 'm-ivy)
  (require 'm-programming))

;; Package for garbage collection modifications
(use-package gcmh
  :diminish
  :config
  (gcmh-mode 1))

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  :commands (esup))

;; Reset gc to 4mb
(setq gc-cons-threshold (* 4 1024 1024)) ; 4mb

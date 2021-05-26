;; m's custom emacs configuration.
;;

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq
  gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6)

(add-hook
 'after-init-hook
 (lambda ()
   (setq
     gc-cons-threshold (* 1024 1024) ;; 1mb
     gc-cons-percentage (car (get 'gc-cons-threshold 'standard-value)))))



(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (require 'm-package)
  (require 'm-backups)
  (require 'm-frame)
  (require 'm-functions)
  (require 'm-project)
  (require 'm-completion-ivy)
  ;; (require 'm-completion-selectrum)
  (require 'm-programming))

;; Package for garbage collection modifications
(use-package gcmh
  :diminish
  :hook (after-init . gcmh-mode))

;; For startup time profiling
(use-package esup
  :pin melpa
  :commands (esup))

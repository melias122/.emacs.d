;; remove noise
(setq inhibit-startup-screen 1)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(line-number-mode 1)

;; maximize emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; TODO: night/day theme
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark 1))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 'first-error) ;; or t

(provide 'm-frame)

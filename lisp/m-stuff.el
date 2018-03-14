;; Stuff that belongs nowhere for now.

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package highlight-parentheses
  :ensure t
  :config (global-highlight-parentheses-mode 1))

(use-package git-gutter-fringe
  :ensure t
  :config (global-git-gutter-mode t))

(use-package magit
  :ensure t)

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode 1)
(show-paren-mode 1)

(setq compilation-scroll-output 'first-error) ;; or t

(use-package thrift
	:ensure t)

(use-package cmake-mode
	:ensure t)

(use-package protobuf-mode
	:ensure t)

(use-package dumb-jump
	:ensure t)

(setq dumb-jump-selector 'ivy)
;; (setq dumb-jump-force-searcher 'ag)
;; (setq dumb-jump-prefer-searcher 'rg)


(provide 'm-stuff)

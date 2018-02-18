(use-package counsel
  :ensure smex
  :bind ("M-x" . counsel-M-x)
  :config
  (smex-initialize)
  (counsel-mode 1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-re-builders-alist '((t  . ivy--regex-ignore-order))))

(provide 'm-ivy)

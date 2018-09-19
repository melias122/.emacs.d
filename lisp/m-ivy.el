(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-re-builders-alist '((t  . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure smex
  :diminish
  :bind ("M-x" . counsel-M-x)
  :config
  (smex-initialize)
  (counsel-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1)

  (use-package counsel-projectile
    :ensure t
    :bind-keymap ("C-c p" . projectile-command-map)
    :init
    ;; fix for counsel-projectile warning when bytecompile
    (require 'subr-x)
    (require 'cl-macs)
    :config
    (counsel-projectile-mode 1)
    (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)))

(provide 'm-ivy)

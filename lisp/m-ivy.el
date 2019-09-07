(use-package ivy
  :ensure t
  :diminish ivy-mode
  :defer 3
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 10)
  (ivy-re-builders-alist '((t  . ivy--regex-ignore-order)))
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode))

(use-package swiper
  :ensure t
  :defer t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :diminish
  :defer t
  :bind ("M-x" . counsel-M-x)
  :config
  (use-package smex
    :ensure t
    :defer t
    :config
    (smex-initialize))
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :defer t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ( ("C-r" . counsel-projectile-rg)
          ("C-k" . counsel-projectile))
  :init
  ;; fix for counsel-projectile warning when bytecompile
  (use-package subr-x)
  (use-package cl-macs)
  :custom
  (projectile-completion-system 'ivy)
  ;; (projectile-project-root-files-bottom-up)
  (counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  (counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  :config
  (counsel-projectile-mode))

(provide 'm-ivy)

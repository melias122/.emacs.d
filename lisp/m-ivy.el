(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-re-builders-alist '((t  . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :ensure t
  :diminish
  :bind ("M-x" . counsel-M-x)
  :config
  (use-package smex
    :ensure t
    :config
    (smex-initialize))
  (counsel-mode))

(use-package counsel-projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ( ("C-r" . counsel-projectile-rg)
          ("C-s" . counsel-grep-or-swiper))
  :init
  ;; fix for counsel-projectile warning when bytecompile
  (require 'subr-x)
  (require 'cl-macs)
  :config
  (setq projectile-completion-system 'ivy)
  ;; (projectile-project-root-files-bottom-up)
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  (counsel-projectile-mode))

(provide 'm-ivy)

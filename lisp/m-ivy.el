(use-package ivy
  :diminish
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-<return>" . ivy-immediate-done))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-height 10)
  (ivy-re-builders-alist '((t  . ivy--regex-ignore-order)))
  (ivy-initial-inputs-alist nil)
  :config
  (ivy-mode 1))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (use-package amx
    :config
    (amx-mode 1))
  (counsel-mode 1))

(use-package counsel-projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-r" . counsel-projectile-rg)
         ("C-k" . counsel-projectile))
  :custom
  (projectile-completion-system 'ivy)
  (counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  :config
  (push "vendor" projectile-globally-ignored-directories)
  (counsel-projectile-mode 1))

(provide 'm-ivy)

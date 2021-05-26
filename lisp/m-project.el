(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("C-k" . projectile-find-file)
  :custom
  (projectile-switch-project-action 'projectile-vc)
  :config
  (push "vendor" projectile-globally-ignored-directories)
  (push "node_modules" projectile-globally-ignored-directories)
  (projectile-mode t))

(provide 'm-project)

(use-package project
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind ("C-k" . project-find-file)
  :config
  (add-to-list 'project-switch-commands '(magit-status "Magit" ?m)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom (magit-bind-magit-project-status nil))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)))

(provide 'm-projects)

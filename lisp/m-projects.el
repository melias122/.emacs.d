(use-package project
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-switch-commands
    (delete '(project-find-file "Find file") project-switch-commands))
  (add-to-list 'project-switch-commands '(magit-status "Magit" ?m))
  (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file" ?f)))


(use-package consult-project-extra
  :bind (("C-k" . consult-project-extra-find)
         :map project-prefix-map
         ("f" . consult-project-extra-find)
         ("o" . consult-project-extra-find-other-window)))
 
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom (magit-bind-magit-project-status nil))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)))

(provide 'm-projects)

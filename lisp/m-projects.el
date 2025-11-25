(use-package magit
  :ensure t
  :custom
  ;; Don't autosave repo buffers. This is too magical, and saving can
  ;; trigger a bunch of unwanted side-effects, like save hooks and
  ;; formatters. Trust the user to know what they're doing.
  (magit-save-repository-buffers nil)

  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (magit-revision-insert-related-refs nil)

  ;; If two projects have the same project name (e.g. A/src and B/src will
  ;; both resolve to the name "src"), Magit will treat them as the same
  ;; project and destructively hijack each other's magit buffers. This is
  ;; especially problematic if you use workspaces and have magit open in
  ;; each, and the two projects happen to have the same name! By unsetting
  ;; `magit-uniquify-buffer-names', magit uses the project's full path as
  ;; its name, preventing such naming collisions.
  (magit-uniquify-buffer-names nil)
  :bind ("C-x g" . magit-status))

(use-package project
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-switch-commands
    (delete '(project-find-file "Find file") project-switch-commands))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
  (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file" ?f)))

(use-package consult-project-extra
  :ensure t
  :bind (("C-k" . consult-project-extra-find)
         :map project-prefix-map
         ("f" . consult-project-extra-find)
         ("o" . consult-project-extra-find-other-window)))

(use-package diff-hl
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)))

(provide 'm-projects)

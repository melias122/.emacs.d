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
  :bind (("C-r" . counsel-projectile-rg)
         ("C-k" . counsel-projectile))
  :custom
  (projectile-completion-system 'ivy)
  (counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-vc)
  :config
  (counsel-projectile-mode 1))

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  :custom
  (xref-prompt-for-identifier nil)
  (xref-search-program 'ripgrep))

(provide 'm-completion-ivy)

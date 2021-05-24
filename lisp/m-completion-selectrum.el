;; Selectrum aims to provide a better completion UI using standard Emacs APIs.
;; In essence it is an interface for selecting items from a list.
(use-package selectrum
  :config
  (selectrum-mode +1))

;; Simple but effective sorting and filtering for Emacs
;; (use-package selectrum-prescient
;;   :config
;;   ;; to make sorting and filtering more intelligent
;;   (selectrum-prescient-mode +1)

;;   ;; to save your command history on disk, so the sorting gets more
;;   ;; intelligent over time
;;   (prescient-persist-mode +1))

;; Orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless))
  ;; Optional performance optimization
  ;; by highlighting only the visible candidates.
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function 'orderless-highlight-matches)
  :config
  ;; Persist history over Emacs restarts
  (savehist-mode))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-ripgrep)
          ("C-x b" . consult-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref)
  (xref-prompt-for-identifier nil)
  (xref-search-program 'ripgrep)
  :config
  (recentf-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("C-k" . projectile-find-file)
  :custom (projectile-switch-project-action 'projectile-vc)
  :config
  (push "vendor" projectile-globally-ignored-directories)
  (projectile-mode 1))

(provide 'm-completion-selectrum)

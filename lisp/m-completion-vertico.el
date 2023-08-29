(use-package vertico
  :ensure t
  :custom
  (vertico-count 8)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Orderless completion style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))
                                   ;; enable initialism by default for symbols
                                   (command (styles +orderless-with-initialism))
                                   (variable (styles +orderless-with-initialism))
                                   (symbol (styles +orderless-with-initialism))))

;; Minibuffer history
(use-package savehist
  :hook (after-init . savehist-mode))

(use-package ctrlf
  :ensure t
  :bind (("C-s"   . ctrlf-forward-fuzzy)
         ("C-M-s" . ctrlf-backward-fuzzy)))

(use-package consult
  :ensure t
  :bind (("C-r"   . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (register-preview-delay 0)
  (register-preview-function 'consult-register-format)

  ;; consult-xref
  (xref-show-xrefs-function       'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  (xref-search-program            'ripgrep)
  (xref-prompt-for-identifier     nil)

  ;; Use project.el with consult
  (consult-project-root-function (lambda ()
                                   (when-let (project (project-current))
                                     (car (project-roots project)))))

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (completion-in-region-function (lambda (&rest args)
                                   (apply (if vertico-mode
                                            #'consult-completion-in-region
                                            #'completion--in-region)
                                     args)))
  :config
  ;; Add previous search when pressing "C-s C-s"
  (defvar m/previous-search
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "C-M-n") (kbd "C-M-p")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
    consult-theme
    :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    ;; preview-key "M-."
    :preview-key '(:debounce 0.4 any)

    consult-line
    :keymap m/previous-search))

(use-package recentf
  :config
  (recentf-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate 'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t))

(provide 'm-completion-vertico)

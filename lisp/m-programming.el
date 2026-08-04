;;
;; syntax
;;

(use-package terraform-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package graphql-mode
  :ensure t
  :mode "\\.graphqls\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.js\\'" . typescript-mode)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.gohtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)))

(use-package prettier
  :ensure t
  :hook ((json-mode . prettier-mode)
         (css-mode . prettier-mode)))

;; cmake syntax highlighting
(use-package cmake-mode
  :ensure t
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("CMakeLists_src\\.txt\\'" . cmake-mode)))

(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; proto syntax highlighting
(use-package protobuf-mode
  :ensure t
  :init
  (defconst m/protobuf-style
    '((c-basic-offset . 2)
       (indent-tabs-mode . nil)))

  (add-hook 'protobuf-mode-hook
    (lambda () (c-add-style "m/protobuf-style" m/protobuf-style t)))

  :mode "\\.proto\\'")

;; yaml syntax highlighting
(use-package yaml-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)))

;; json syntax highlighting
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package dockerfile-mode
  :ensure t
  :defer t)

;;
;; editing
;;
(use-package ws-butler
  :ensure t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package editorconfig
  :ensure t
  :delight " EC"
  :hook (after-init . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  (tab-width 4)
  :config
  ;; https://github.com/doomemacs/doomemacs/blob/master/modules/tools/editorconfig/config.el
  ;; (add-to-list 'editorconfig-exclude-regexps
  ;; "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")
  )

(use-package simple
  :custom (backward-delete-char-untabify-method nil)
  :config
  (line-number-mode t)
  (column-number-mode t))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

(use-package idle-highlight-mode
  :ensure t
  :hook (prog-mode . idle-highlight-mode))

(use-package electric
  :hook (prog-mode . electric-pair-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode))

;;
;; Language Server Protocol (LSP)
;;
(use-package eglot
  :init
  (defun m/eglot-go-before-save ()
    (when (eglot-managed-p)
      (eglot-format-buffer)
      ;; `eglot-code-action-organize-imports' signals an error when gopls
      ;; offers no action (imports already organized); don't abort the save
      (ignore-errors
        (eglot-code-action-organize-imports (point-min) (point-max)))))

  (defun m/eglot-go-install-save-hooks ()
    (add-hook 'before-save-hook #'m/eglot-go-before-save t t))
  :hook (((go-mode
           go-dot-mod-mode
           go-dot-work-mode
           typescript-mode
           javascript-mode
           csharp-mode
           python-mode
           zig-mode) . eglot-ensure)
         (go-mode . m/eglot-go-install-save-hooks))
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l o" . eglot-code-action-organize-imports)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l t" . eglot-find-typeDefinition)
              ("M-i"     . eglot-find-implementation)))

(use-package consult-eglot
  :ensure t
  :after eglot
  :bind (:map eglot-mode-map ([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package flycheck
  :ensure t
  :custom
  ;; No checker supports *scratch*: emacs-lisp-checkdoc doesn't handle
  ;; lisp-interaction-mode, and the byte-compile checker needs a trusted file.
  (flycheck-global-modes '(not lisp-interaction-mode))
  :init (global-flycheck-mode))

;; Route eglot diagnostics through flycheck instead of flymake
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :mode (("\\go.mod\\'"  . go-dot-mod-mode)
         ("\\go.work\\'" . go-dot-work-mode))
  :bind (:map go-mode-map ("TAB" . m/indent-or-insert-tab)))

;;
;; c/c++-mode
;;
(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (backward-delete-char-untabify-method nil))

;; c++11 and beyond syntax highlighting
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;;
;; jump
;;
(use-package dumb-jump
  :ensure t
  :init
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-default-project "~/code")
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac x ns))
  :hook (after-init . exec-path-from-shell-initialize))

(use-package restclient
  :ensure t
  :defer t)

;; Compilation output
(setq compilation-scroll-output t)

(provide 'm-programming)

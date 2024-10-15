;;
;; syntax
;;

(use-package terraform-mode
  :ensure t)

(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'")

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'")

(use-package graphql-mode
  :ensure t
  :mode "\\.graphqls\\'"
  :config
  (setq-local indent-line-function nil))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.js\\'" . typescript-mode))
  :config
  (define-derived-mode tsx-mode typescript-mode "tsx-mode"))

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
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :config
  (use-package flymd :ensure t))

;; json syntax highlighting
(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package dockerfile-mode
  :ensure t)

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
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'"))

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
  :ensure t
  :hook (after-init . global-hl-line-mode))

;;
;; completion & snippets
;;
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode-on))

(use-package yasnippet-snippets
  :ensure t)

(use-package consult-yasnippet
  :ensure t
  :init
  (defun m/setup-consult-yas-capf ()
    (setq-local completion-at-point-functions
      (cons #'consult-yasnippet
        completion-at-point-functions)))
  (add-hook 'prog-mode-hook 'm/setup-consult-yas-capf))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode))

;;
;; Language Server Protocol (LSP)
;;
(use-package lsp-mode
  :ensure t
  :init
  (with-eval-after-load 'lsp-mode
    ;; do not watch go vendor directory
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'"))

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ((go-mode
           go-dot-mod-mode
           go-dot-work-mode
           typescript-mode
           javascript-mode
           csharp-mode
           python-mode
           zig-mode) . lsp)
         (go-mode . lsp-go-install-save-hooks))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-snippet nil))

(use-package consult-lsp
  :ensure t
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; dap-mode emacs-lsp.github.io/dap-mode
;; GO requires https://github.com/go-delve/delve/tree/master/Documentation/installation
(use-package dap-mode
  :ensure t
  :hook (go-mode . (lambda () (require 'dap-dlv-go))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :mode (("\\go.mod\\'"  . go-dot-mod-mode)
         ("\\go.work\\'" . go-dot-work-mode))
  :bind ("TAB" . m/indent-or-insert-tab))

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
  :ensure t)

;; Compilation output
(setq compilation-scroll-output t)

(provide 'm-programming)

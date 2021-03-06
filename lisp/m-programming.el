;;
;; syntax
;;

(use-package vue-mode)
(use-package csharp-mode)

;; cmake syntax highlighting
(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("CMakeLists_src\\.txt\\'" . cmake-mode)))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; proto syntax highlighting
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; yaml syntax highlighting
(use-package yaml-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :config
  (use-package flymd))

;; json syntax highlighting
(use-package json-mode)

;;
;; editing
;;
(use-package editorconfig
  :ensure nil
  :delight " EC"
  :defer 1
  :config (editorconfig-mode 1))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :config
  (line-number-mode 1))

(use-package highlight-parentheses
  :defer 1
  :diminish
  :config (global-highlight-parentheses-mode 1))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package electric
  :hook (prog-mode . electric-pair-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package hl-line
  :defer 1
  :config (global-hl-line-mode 1))

;;
;; compile
;;
(use-package compile
  :custom (compilation-scroll-output 'first-error))

;;
;; git
;;
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :defer 1
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode))

;;
;; completion & snippets
;;
(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package company
  :bind (("C-M-i" . counsel-company)
          :map company-active-map
          ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
          ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1))))
  :custom
  (company-idle-delay nil) ; removes popup
  (company-echo-delay 0)   ; removes blinking
  :config
  (global-company-mode 1)
  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-xcode company-backends)))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode))

;;
;; Language Server Protocol (LSP)
;;
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet nil)
  (lsp-file-watch-threshold 128000)
  (lsp-diagnostic-package :auto)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-xref t)
  :config
  (lsp-register-custom-settings '(("gopls.staticcheck" t t))))

;;
;; go-mode
;;
(use-package go-mode
  :init
  (defun m/go-mode-hooks ()
    (add-hook 'before-save-hook 'lsp-format-buffer t t)
    (add-hook 'before-save-hook 'lsp-organize-imports t t))
  :hook ( (go-mode . lsp-deferred)
          (go-mode . m/go-mode-hooks)
          (go-mode . (lambda () (local-set-key (kbd "TAB") m/indent-or-insert-tab)))))

(use-package eglot
  :disabled
  :hook ((c-mode c++-mode go-mode) . (lambda () (eglot-ensure)))
  :custom (company-transformers nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :disabled
  :hook ((c-mode c++-mode) . lsp-deferred)
  :custom (ccls-sem-highlight-method 'overlay))

(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (backward-delete-char-untabify-method nil))

;; c++11 and beyond syntax highlighting
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;
;; jump
;;
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-selector 'ivy))

(use-package exec-path-from-shell
  :defer 1
  :if (memq window-system '(mac x ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(provide 'm-programming)

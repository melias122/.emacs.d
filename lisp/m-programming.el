;;
;; syntax
;;
(use-package thrift
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :config
  (use-package flymd
    :ensure t))

(use-package yaml-mode
  :ensure t)

;;
;; editing
;;
(use-package editorconfig
  :ensure t
  :diminish
  :config (editorconfig-mode 1))

(use-package simple
  :hook ('before-save . delete-trailing-whitespace))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :config (global-highlight-parentheses-mode 1))

(use-package idle-highlight-mode
  :ensure t
  :diminish
  :hook (prog-mode . (lambda ()
                       (unless (memq major-mode '(c-mode c++-mode objc-mode))
                         (idle-highlight-mode)))))

(use-package electric
  :config (electric-pair-mode 1))

(use-package paren
  :config (show-paren-mode 1))

(use-package hl-line
  :config (global-hl-line-mode 1))

;;
;; git
;;
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode t))

(use-package magit
  :ensure t
  :diminish
  :bind (("C-x g" . magit-status)))

;;
;; completion & snippets
;;
(use-package yasnippet
  :disabled
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package company
  :ensure t
  :bind (("C-M-i" . company-indent-or-complete-common)
          :map company-active-map
          ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
          ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1))))
  :custom
  (company-idle-delay 1) ; popup delay
  (company-echo-delay 0) ; removes blinking
  :config
  (global-company-mode t))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports")
  :config
  (use-package go-eldoc
    :ensure t)
  (use-package go-guru
    :ensure t)
  (use-package company-go
    :ensure t))

;;
;; Language Server Protocol (LSP)
;;
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode go-mode) . lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake :none)
  (lsp-enable-snippet nil)
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil)))

(use-package eglot
  :disabled t
  :ensure t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (eglot-ensure)))
  :custom (company-transformers nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :ensure t
  :custom (ccls-sem-highlight-method 'overlay))

;;
;; jump
;;
(use-package ivy-xref
  :ensure t
  :custom (xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package ag ;; for smart-jump references
  :ensure t)

(use-package smart-jump
  :ensure t
  :custom (dumb-jump-selector 'ivy)
  :config (smart-jump-setup-default-registers))

(provide 'm-programming)

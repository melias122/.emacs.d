;;
;; syntax
;;
(use-package thrift
  :ensure t
  :defer t)

(use-package cmake-font-lock
  :ensure t
  :defer t
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :ensure t
  :defer t
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :config
  (use-package flymd
    :ensure t))

(use-package yaml-mode
  :ensure t
  :defer t)

;;
;; editing
;;
(use-package editorconfig
  :ensure t
  :diminish
  :defer t
  :custom
  (default-tab-width 4)
  :config (editorconfig-mode 1))

(use-package simple
  :hook ('before-save . delete-trailing-whitespace)
  :config
  (line-number-mode 1))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :defer 5
  :config (global-highlight-parentheses-mode 1))

(use-package idle-highlight-mode
  :ensure t
  :diminish
  :defer 5
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
;; compile
;;
(use-package compile
 :custom
 (compilation-scroll-output 'first-error))

;;
;; git
;;
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :defer 5
  :config (global-git-gutter-mode t))

(use-package magit
  :ensure t
  :diminish
  :defer 3
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
  :defer 5
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
  :diminish eldoc-mode
  :defer 5)

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :defer t
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
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode go-mode) . lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake :none)
  (lsp-enable-snippet nil)
  :config
  (use-package company-lsp
    :ensure t
    :commands company-lsp
    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil)))

(use-package eglot
  :disabled
  :ensure t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (eglot-ensure)))
  :custom (company-transformers nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :ensure t
  :defer t
  :custom (ccls-sem-highlight-method 'overlay))

;;
;; jump
;;
(use-package ivy-xref
  :ensure t
  :defer t
  :custom (xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package ag ;; for smart-jump references
  :ensure t
  :defer t)

(use-package smart-jump
  :ensure t
  :defer t
  :custom (dumb-jump-selector 'ivy)
  :config (smart-jump-setup-default-registers))

(provide 'm-programming)

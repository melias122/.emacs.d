;;
;; syntax
;;
(use-package thrift :diminish)
(use-package cmake-font-lock :hook (cmake-mode . cmake-font-lock-activate))
(use-package cmake-mode :mode ("CMakeLists_src.txt"))
(use-package protobuf-mode :defer t)
(use-package yaml-mode)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode ( ("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown")
  :config
  (use-package flymd))

;;
;; editing
;;
(use-package editorconfig
  :diminish
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
(use-package git-gutter-fringe
  :defer 1
  :diminish git-gutter-mode
  :config (global-git-gutter-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status))

;;
;; completion & snippets
;;
(use-package yasnippet
  :disabled
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package company
  :defer 0.5
  :diminish
  :bind (("C-M-i" . company-indent-or-complete-common)
          :map company-active-map
          ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
          ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1))))
  :custom
  (company-idle-delay 1) ; popup delay
  (company-echo-delay 0) ; removes blinking
  :config
  (global-company-mode 1))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode))

;;
;; go-mode
;;
(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))

;;
;; Language Server Protocol (LSP)
;;

;; On fresh install emacs is complaining that lsp/ccls needs spinner
(use-package spinner)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode go-mode) . lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake :none)
  (lsp-enable-snippet nil)
  (lsp-file-watch-threshold 128000)
  :config
  (use-package company-lsp
    :commands company-lsp
    :custom
    (company-transformers nil)
    (company-lsp-async t)
    (company-lsp-cache-candidates nil)))

(use-package eglot
  :disabled
  :hook ((c-mode c++-mode go-mode) . (lambda () (eglot-ensure)))
  :custom (company-transformers nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :custom (ccls-sem-highlight-method 'overlay))

(use-package cc-mode
  :custom
  (c-default-style "linux")
  (c-basic-offset 4)
  (backward-delete-char-untabify-method nil))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

;;
;; jump
;;
(use-package ivy-xref
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package smart-jump
  :defer 1
  :custom
  (dumb-jump-selector 'ivy)
  (smart-jump-find-references-fallback-function 'm/smart-jump-find-references-with-counsel-rg)
  :config
  (smart-jump-setup-default-registers))

(use-package exec-path-from-shell
  :defer 1
  :if (memq window-system '(mac x ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(provide 'm-programming)

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
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package flymd
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;
;; editing
;;
(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

(use-package simple
  :hook ('before-save . delete-trailing-whitespace))

(use-package highlight-parentheses
  :ensure t
  :diminish
  :config
  (global-highlight-parentheses-mode 1))

(use-package idle-highlight-mode
  :ensure t
  :diminish
  :hook (prog-mode . idle-highlight-mode))

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
  :bind (("C-x g" . magit-status))
  :diminish)

;;
;; completion & snippets
;;
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

(use-package company
  :ensure t
  :bind (("C-M-i" . company-indent-or-complete-common)
          :map company-active-map
          ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
          ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1))))
  ;; https://onze.io/emacs/c++/2017/03/16/emacs-cpp.html
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or
          (not company-mode/enable-yas)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
      (append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))

  :config
  (global-company-mode t)

  (setq company-idle-delay 1) ; popup delay
  (setq company-echo-delay 0) ; removes blinking
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode)

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (use-package go-eldoc
    :ensure t)
  (use-package go-guru
    :ensure t))

(use-package company-go
  :ensure t)

;;
;; Language Server Protocol (LSP)
;;

;; fix for ccls/lsp
(use-package f :ensure t)
(use-package ht :ensure t)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-eldoc-render-all nil)
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  (lsp-prefer-flymake :none))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :custom
  (company-transformers nil)
  (company-lsp-async t)
  (company-lsp-cache-candidates nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))
  :init
  (setq ccls-executable "/usr/local/bin/ccls"))

;;
;; jump
;;
(use-package ivy-xref
  :ensure t
  :custom
  (xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package ag ;; for smart-jump references
  :ensure t)

(use-package smart-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (smart-jump-setup-default-registers)
  ;; (smart-jump-register :modes '(lsp-mode)
  ;;   :jump-fn 'lsp-find-definition
  ;;   :pop-fn 'pop-tag-mark
  ;;   :refs-fn 'lsp-find-references
  ;;   :heuristic 'point
  ;;   :async 500
  ;;   :order 1)

  (setq smart-jump-lsp-mode-order 1)
  (smart-jump-lsp-mode-register))

(provide 'm-programming)

;;
;; syntax
;;

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
  :disabled
  :diminish yas-minor-mode
  :config (yas-global-mode t))

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

;; On fresh install emacs is complaining that lsp/ccls needs spinner
(use-package spinner)

(use-package lsp-mode
  :disabled
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet nil)
  (lsp-file-watch-threshold 128000)
  (lsp-diagnostic-package :none)
  (lsp-headerline-breadcrumb-enable nil))

(use-package company-lsp
  :disabled
  :commands company-lsp
  :custom
  (company-transformers nil)
  (company-lsp-async t)
  (company-lsp-cache-candidates nil))

;;
;; go-mode
;;
(use-package go-mode
  :init

  ;; make project understand go modules
  (require 'project)

  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)

  ;; Optional: install eglot-format-buffer as a save hook.
  ;; The depth of -10 places this before eglot's willSave notification,
  ;; so that that notification reports the actual contents that will be saved.
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

  ;; eglot-organize-imports is hopefully a temporary stopgap until
  ;; https://github.com/joaotavora/eglot/issues/574 is addressed.
  (defun eglot-organize-imports ()
    "Offer to execute the source.organizeImports code action."
    (interactive)
    (unless (eglot--server-capable :codeActionProvider)
      (eglot--error "Server can't execute code actions!"))
    (let* ((server (eglot--current-server-or-lose))
            (actions (jsonrpc-request
                       server
                       :textDocument/codeAction
                       (list :textDocument (eglot--TextDocumentIdentifier))))
            (action (cl-find-if
                      (jsonrpc-lambda (&key kind &allow-other-keys)
                        (string-equal kind "source.organizeImports" ))
                      actions)))
      (when action
        (eglot--dcase action
          (((Command) command arguments)
            (eglot-execute-command server (intern command) arguments))
          (((CodeAction) edit command)
            (when edit (eglot--apply-workspace-edit edit))
            (when command
              (eglot--dbind ((Command) command arguments) command
                (eglot-execute-command server (intern command) arguments))))))))

  (defun eglot-organize-imports-on-save ()
    (defun eglot-organize-imports-nosignal ()
      "Run eglot-organize-imports, but demote errors to messages."
      ;; Demote errors to work around
      ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
      ;; so that we do not prevent subsequent save hooks from running
      ;; if we encounter a spurious error.
      (with-demoted-errors "Error: %s" (eglot-organize-imports)))
    (add-hook 'before-save-hook #'eglot-organize-imports-on-save))

  (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)

  :hook ( (go-mode . eglot-ensure)
          (go-mode . eglot-format-buffer-on-save)))

(use-package eglot
  :hook ((c-mode c++-mode) . (lambda () (eglot-ensure)))
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
(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

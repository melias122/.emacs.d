;;
;; syntax
;;

(use-package vue-mode
  :mode "\\.vue\\'")
(use-package csharp-mode
  :mode "\\.cs\\'")

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
(use-package json-mode
  :mode "\\.json\\'")

(use-package dockerfile-mode)

;;
;; editing
;;
(use-package editorconfig
  :delight " EC"
  :hook (after-init . editorconfig-mode))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom (backward-delete-char-untabify-method nil)
  :config
  (line-number-mode 1))

(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

(use-package idle-highlight-mode
  :hook (prog-mode . idle-highlight-mode))

(use-package electric
  :hook (prog-mode . electric-pair-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

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
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode)))

;;
;; completion & snippets
;;
(use-package yasnippet
  :disabled
  :init
  (use-package yasnippet-snippets)
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode-on))

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
(use-package eglot
  :commands (eglot eglot-ensure)
  :hook (((c-mode c++-mode go-mode) . eglot-ensure)))

;;
;; go-mode
;;
(use-package go-mode
  :bind ("TAB" . m/indent-or-insert-tab)
  :hook ((before-save . (lambda () (call-interactively 'eglot-code-action-organize-imports)))
          (before-save . eglot-format-buffer)))

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
  :hook (c++-mode . modern-c++-font-lock-mode))

;;
;; jump
;;
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)
  :custom
  (dumb-jump-selector ((lambda () (if (featurep 'ivy) 'ivy 'completing-read)))))

(use-package exec-path-from-shell
  :if (memq window-system '(mac x ns))
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'm-programming)

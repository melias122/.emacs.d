;;
;; syntax
;;

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

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
  :hook (after-init . editorconfig-mode)
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode))

(use-package simple
  :ensure nil
  :custom (backward-delete-char-untabify-method nil)
  :config
  (line-number-mode t)
  (column-number-mode t))

(use-package ws-butler
  :diminish
  :hook (after-init . ws-butler-global-mode))

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
;; completion & snippets
;;
(use-package yasnippet
  :disabled
  :init
  (use-package yasnippet-snippets)
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode-on))

(use-package eldoc
  :diminish
  :hook (prog-mode . eldoc-mode))

;;
;; Language Server Protocol (LSP)
;;
(use-package eglot
  :commands (eglot eglot-ensure)
  :hook (((c-mode          ;; clangd
           c++-mode        ;; clangd
           go-mode         ;; go install golang.org/x/tools/gopls@latest

           ;; web
           html-mode       ;; npm i -g vscode-html-languageserver-bin
           css-mode        ;; npm i -g vscode-css-languageserver-bin
           svelte-mode     ;; npm i -g svelteserver
           javascript-mode ;; npm i -g typescript-language-server
           typescript-mode ;; npm i -g ypescript-language-server

           json-mode       ;; npm i -g vscode-json-languageserver
           ) . eglot-ensure))
  :config
  (add-to-list 'eglot-stay-out-of 'company)
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-server-programs '(svelte-mode . ("svelteserver"  "--stdio"))))

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
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-default-project "~/code")
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package exec-path-from-shell
  :if (memq window-system '(mac x ns))
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'm-programming)

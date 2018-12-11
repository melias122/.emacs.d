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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode 1))

(use-package idle-highlight-mode
  :ensure t
  :diminish
  :hook (prog-mode . idle-highlight-mode))

(electric-pair-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)

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
  :bind (:map company-active-map
          ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))
          ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))

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

  (setq company-idle-delay nil)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                            ; remove annoying blinking

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
  :init
  ;; (setf lsp-eldoc-render-all nil)
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  :config
  (require 'lsp-clients))

(use-package lsp-ui
  :disabled t
  :ensure t
  :after (lsp)
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setf lsp-ui-sideline-enable nil)
  (setf lsp-ui-doc-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :init
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))

;;
;; c/c++-mode
;;
(use-package ccls
  :ensure t
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp)))
  :init
  ;; TODO(m): Fix absolute HOME
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq ccls-initialization-options '( :clang (:excludeArgs ("-mthumb-interwork"
                                                              "-march=armv7-a"
                                                              "-mfpu=neon"
                                                              "-mfloat-abi=hard"
                                                              "-fno-inline-small-functions")
                                                :pathMappings ( "/root/src/github.com/bang-olufsen/ase:/home/m/code/beo/ase"
                                                                "/sysroots/beoase2gvas810/usr/include:/home/m/code/beo/ase/include"
                                                                "/sysroots/beoase2s810/usr/include:/home/m/code/beo/ase/include"
                                                                "/sysroots/beoase/usr/include:/home/m/code/beo/ase/include"))
                                       :index (:comments 2)
                                       :completion (:detailedLabel t)))
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
      (append '("compile_commands.json")
        projectile-project-root-files-top-down-recurring))))

;;
;; jump
;;
(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  (setq dumb-jump-selector 'ivy))

(provide 'm-programming)

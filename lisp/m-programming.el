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
  :diminish
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
          ("C-p" . company-select-previous)
          ("C-n" . company-select-next))

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


  (setq company-tooltip-limit 20)                        ; bigger popup window
  (setq company-idle-delay .5)                           ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                            ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))   ; start autocompletion only after typing

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

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
;; lsp
;;
(use-package lsp-mode
  :ensure t
  :config
  (use-package company-lsp
    :ensure t
    :config
    (push 'company-lsp company-backends))

  (use-package lsp-ui
    :ensure t
    :hook (lsp-mode . lsp-ui-mode)))

;;
;; c/c++-mode
;;
(use-package ccls
  :ensure t
  :init
  (setq ccls-executable "/usr/local/bin/ccls")
  (setq ccls-extra-init-params '(:clang (:excludeArgs ("-mthumb-interwork"
                                                        "-march=armv7-a"
                                                        "-mfpu=neon"
                                                        "-mfloat-abi=hard"
                                                        "-fno-inline-small-functions")
                                          :pathMappings ("/root/src/github.com/bang-olufsen/ase:/Users/m/code/beo/ase"))
                                  :index (:comments 2)
                                  :completion (:detailedLabel t)))
  :hook ((c-mode c++-mode) . lsp-ccls-enable)

  :config
  (setq ccls-sem-highlight-method 'font-lock)
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")))

(use-package cquery
  :disabled t
  :ensure t
  :init
  (setq cquery-executable "/usr/local/bin/cquery")
  :hook ((c-mode c++-mode) . #'lsp-cquery-enable)
  :config
  (setq cquery-sem-highlight-method 'font-lock))

;;
;; jump
;;
(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  (setq dumb-jump-selector 'ivy))

(provide 'm-programming)

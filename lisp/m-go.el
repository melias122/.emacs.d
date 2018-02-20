(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize))

(use-package go-eldoc
  :defer t
  :ensure t)

(use-package go-guru
  :defer t
  :ensure t)

(use-package go-mode
  :ensure t

  :init
  (setq gofmt-command "goimports")
  (setenv "GOPATH" (concat (getenv "HOME") "/code/go"))

  (add-hook 'before-save-hook 'gofmt-before-save))

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(provide 'm-go)

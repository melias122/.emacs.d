(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package go-eldoc
  :ensure t)

(use-package go-guru
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

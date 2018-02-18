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
  :mode "\\.go\\'"

  :init
  (setq gofmt-command "goimports")
  (setenv "GOPATH" (concat (getenv "HOME") "/code/go"))

  :bind (("M-." . godef-jump)
	 ("M-," . pop-tag-mark))

  :config
  (defun m-go-mode ()
  "My personal go-mode setup"
  
  (go-eldoc-setup)
  (electric-pair-mode 1)
  (auto-complete-mode 1)
  (highlight-parentheses-mode 1))
  
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (add-hook 'go-mode-hook 'm-go-mode))


;; (setenv "GOROOT" (concat (getenv "HOME") "/go1.10"))

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(provide 'm-go)

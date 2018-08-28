;;
;; env
;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (exec-path-from-shell-copy-env "GOPATH"))

;;
;; go-mode
;;
(use-package go-mode
  :ensure t
  :init
  (setq gofmt-command "goimports")

  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :ensure t)

(use-package go-guru
  :ensure t)

(use-package company-go
	:ensure t)

(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;;
;; c/c++-mode
;;
(use-package rtags
  :ensure t)

(use-package ivy-rtags
  :ensure t)

(use-package company-rtags
  :ensure t)

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(use-package flycheck-rtags
  :ensure t)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;;
;; jump
;;
(use-package smart-jump
	:ensure t
	:config
  (smart-jump-setup-default-registers)

  (smart-jump-register
    :modes '(c-mode c++-mode)
    :jump-fn 'rtags-find-symbol-at-point
    :pop-fn 'rtags-location-stack-back
    :refs-fn 'rtags-find-all-references-at-point
    :should-jump (lambda ()
                   (and
                     (fboundp 'rtags-executable-find)
                     (rtags-executable-find "rc")
                     (rtags-is-indexed)))
    :heuristic 'point
    :async 500
    :order 1)

  (setq dumb-jump-selector 'ivy))

(provide 'm-programming)

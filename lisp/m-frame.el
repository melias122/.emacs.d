(use-package frame
  :init

  :bind ( ("C-z" . undo)
          ("C-x C-z" . nil)
          ("M-g" . goto-line)
          ("C-/" . comment-or-uncomment-line-or-region))
  :custom
  (inhibit-startup-screen 1)
  :config
  (when (memq window-system '(mac ns))
    (set-default-font "Monaco 13")
    ;; Make mac keyboard bindings not suck
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta))

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; maximize emacs on start
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config
  (setq-default save-place t))

;; TODO: night/day theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one 1)

  (use-package solaire-mode
    :ensure t
    :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
            (ediff-prepare-buffer . solaire-mode)
            (minibuffer-setup . solaire-mode-in-minibuffer))
    :config
    (solaire-mode-swap-bg)))

(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :config
  (which-key-mode))

;;  paradox is modernized package menu
(use-package paradox
  :ensure t
  :defer 2
  :config
  (paradox-enable))

(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  (tramp-histfile-override "/tmp/.tramp_history")
  (tramp-chunksize 2000))

(use-package delsel
  :bind (:map mode-specific-map
          ("C-g" . minibuffer-keyboard-quit)))

;; this should be fixed in 26.3+
(use-package gnutls
  :custom
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


(provide 'm-frame)

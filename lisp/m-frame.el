(use-package frame
  :ensure nil
  :defer nil
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
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (push '(tool-bar-lines . 0) default-frame-alist)
  ;; (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (display-graphic-p)
    (tool-bar-mode -1))
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :config
  (setq-default save-place t))

;; TODO: night/day theme
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Defaults
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package which-key
  :diminish
  :defer 1
  :config
  (which-key-mode 1))

(use-package tramp
  :defer t
  :custom
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil)
  (tramp-histfile-override "/tmp/.tramp_history")
  (tramp-chunksize 2000)
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp")))

(use-package delsel
  :bind (:map mode-specific-map
          ("C-g" . minibuffer-keyboard-quit)))

;; this should be fixed in 26.3+
(use-package gnutls
  :custom
  (gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; for dired
(setq dired-dwim-target t)

(provide 'm-frame)

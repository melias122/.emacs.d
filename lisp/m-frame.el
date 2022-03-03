(use-package frame
  :ensure nil
  :demand t
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

  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :hook (after-init . save-place-mode))

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
  :hook (after-init . which-key-mode))

(use-package delsel
  :bind (:map mode-specific-map
          ("C-g" . minibuffer-keyboard-quit)))

;; for dired
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)

  ;; Revert/refresh dired when something changes
  (global-auto-revent-non-file-buffers t))

;; Revert buffers when underlying file changed
(global-auto-revert-mode 1)

;; Will flash cursor on big move, like mouse scroll
(use-package beacon
  :hook (after-init . beacon-mode))

(provide 'm-frame)

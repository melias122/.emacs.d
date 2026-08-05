(use-package frame
  :demand t
  :bind ( ("C-z" . undo)
          ("C-x C-z" . nil)
          ("C-x C-c" . nil)
          ("M-g" . goto-line)
          ("C-/" . comment-or-uncomment-line-or-region))
  :custom
  (inhibit-startup-screen 1)
  :config
  (when (memq window-system '(mac ns))
    ;; Make mac keyboard bindings not suck
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta))

  (defalias 'yes-or-no-p 'y-or-n-p))

;; Predictable buffer placement:
;; - automatic splits are always side-by-side (when the window is wide
;;   enough), never below; otherwise an existing window is reused
;; - transient buffers (help, compilation/grep, flycheck list, eldoc)
;;   share a single bottom panel instead of popping up arbitrarily
(use-package window
  :custom
  (split-height-threshold nil)
  (split-width-threshold 160)
  ;; make C-x b and friends follow display-buffer-alist too
  (switch-to-buffer-obey-display-actions t)
  ;; side windows are dedicated; pop elsewhere instead of erroring when
  ;; switching buffers from one
  (switch-to-buffer-in-dedicated-window 'pop)
  ;; focus the help window on open, so q closes it right away (help.el var)
  (help-window-select t)
  (display-buffer-alist
   '(((or (derived-mode . help-mode)
          (derived-mode . compilation-mode) ; grep-mode derives from it
          (derived-mode . flycheck-error-list-mode)
          "\\`\\*eldoc")
      (display-buffer-in-side-window)
      (side . bottom)
      (window-height . 0.3)))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(use-package saveplace
  :hook (after-init . save-place-mode))

;; TODO: night/day theme
(use-package doom-themes
  :ensure t
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
  :custom
  (dired-dwim-target t)

  ;; Revert/refresh dired when something changes
  (global-auto-revert-non-file-buffers t))

;; Revert buffers when underlying file changed
(global-auto-revert-mode 1)

;; Will flash cursor on big move, like mouse scroll
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

(use-package shell-pop
  :ensure  t
  :bind ("C-<return>" . shell-pop))

(provide 'm-frame)

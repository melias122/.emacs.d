;; remove noise
(setq inhibit-startup-screen 1)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(line-number-mode 1)

;; maximize emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

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
    :hook ( ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
            (ediff-prepare-buffer . solaire-mode)
            (minibuffer-setup . solaire-mode-in-minibuffer))
    :config
    (solaire-mode-swap-bg)))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 'first-error) ;; or t

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;; set default with to 4 instead of 8
(setq default-tab-width 4)

(provide 'm-frame)

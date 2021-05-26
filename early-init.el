;;; Emacs GUI
(unless (display-graphic-p)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; maximize emacs on start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; This font can be installed from `https://github.com/IBM/plex'
(add-to-list 'default-frame-alist
  '(font . "IBM Plex Mono 11"))

;;; [ Debug ]
;; for Emacs startup freeze debug.
(setq debug-on-quit t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-quit nil)))
(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

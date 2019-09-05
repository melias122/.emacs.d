;; m's custom emacs configuration.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
	(require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac x ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (require 'm-backups)
  (require 'm-frame)
  (require 'm-keybindings)
  (require 'm-fonts)
  (require 'm-ivy)
  (require 'm-json)
  (require 'm-programming))

;; put emacs custom-set-variables to separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; package to try (suggested by Fionn)
;;
;; idle-highlight-mode
;; which-key
;; expand-region
;; iedit
;; visual-regexp
;; avy
;; hydra
;; find-file-in-project (only really need "find-file-in-current-directory", very useful outside repos)
;; ws-butler
;; wgrep
;; all

(defun compile-streamsdk ()
  "Compile StreamSDK."
  (interactive)
  (require 'magit)
  (let ((project-dir (magit-toplevel)))
    (if (null project-dir)
        (message "Not in a project directory!")
      (let ((build (completing-read "build type: " (directory-files (magit-toplevel) t "\.build-.*$") nil t)))
        (switch-to-buffer "*compilation*")
        (cd build)
        (compile "make -j 7")))))

;;  paradox is modernized package menu
(use-package paradox
  :ensure t
  :defer 1
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
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

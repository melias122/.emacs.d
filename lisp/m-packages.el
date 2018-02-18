

;; (defvar m-pkgs
;;   '(atom-one-dark-theme
;;     company
;;     company-go
;;     dockerfile-mode
;;     exec-path-from-shell
;;     go-autocomplete
;;     go-eldoc
;;     go-errcheck
;;     go-guru
;;     go-mode
;;     go-rename
;;     highlight-parentheses
;;     magit
;;     neotree
;;     projectile
;;     ))

;; (setq package-archives
;;       '(("gnu" . "https://elpa.gnu.org/packages/")
;; 	("melpa" . "https://melpa.org/packages/")))

;; (package-initialize)

;; (defun m-pkgs-installed-p ()
;; 	"Return t if all required packages are installed."
;;   (cl-reduce (lambda (a b) (and a b))
;; 	     (mapcar 'package-installed-p m-pkgs)))

;; (defun m-pkgs-install-deps ()
;; 	"Install all package dependencies required by my configuration."
;;   (unless (m-pkgs-installed-p)
;;     (message "%s" "Now refershing package database...")
;;     (package-refresh-contents)
;;     (message "%s" " done.")
;;     (dolist (p m-pkgs)
;;       (when (not (package-installed-p p))
;; 	(package-install p)))))

;; (m-pkgs-install-deps)

(provide 'm-packages)

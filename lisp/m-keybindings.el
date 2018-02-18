;; (require 'ido)

;;(setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; Make mac keyboard bindings not suck
(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; Don't suspend frames
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; (global-set-key "\M-c" 'copy-region-as-kill)
;; (global-set-key "\M-v" 'yank)
(global-set-key "\M-g" 'goto-line)

;; Comments
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/") 'comment-or-uncomment-line-or-region)

;; Get undo back
(global-set-key (kbd "C-z") 'undo)

(provide 'm-keybindings)

(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun m/smart-jump-find-references-with-counsel-rg ()
  "Use `rg' and `counsel' to find references."
  (interactive)
  (if (fboundp 'counsel-rg)
    (counsel-rg
      (cond ((use-region-p)
              (buffer-substring-no-properties (region-beginning)
                (region-end)))
        ((symbol-at-point)
          (substring-no-properties
            (symbol-name (symbol-at-point))))))
    (message "Install swiper to use `+smart-jump-simple-find-references-with-counsel-rg'.")))

(defun m/indent-or-insert-tab (arg)
  "Insert TAB if point is in a string, otherwise call `indent-for-tab-command'."
  (interactive "P")
  (if (nth 3 (syntax-ppss (point)))
    (insert "\t")
    (indent-for-tab-command arg)))

(provide 'm-functions)

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

(provide 'm-functions)

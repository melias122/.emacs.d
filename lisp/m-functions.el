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

(provide 'm-functions)

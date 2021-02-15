(defun peterc/chronos-open-another-buffer ()
  (interactive)
  (split-window-below)
  (save-excursion
    (other-window 1)
    (shrink-window 6)
    (switch-to-buffer "*chronos*")
    (other-window 1)))

;; remove org links in export 
;; %s#(\[\(.*\)?|.*?\])#\1#
;; replace match is replacing the whole line
(defun peterc/reduce-org-link ()
  (interactive)
  (let ((case-fold-search t))		; or nil

    (goto-char (point-min))
    (while (re-search-forward "(\\[\(.*?\)\\|.*?\\])" nil t)
      (replace-match "dummy"))

    ;; repeat for other string pairs
    ))



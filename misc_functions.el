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



;; turn
;; josh
;; sam
;; jed
;; C.J.
;; toby
;; to
;; "josh", "jed", "sam", "C.J.", "toby"
;; source https://news.ycombinator.com/item?id=22129636
(defun peterc/arrayify (start end quote)
"Turn strings on newlines into a QUOTEd, comma-separated one-liner."
(interactive "r\nMQuote: ")
(let ((insertion
	(mapconcat
	(lambda (x) (format "%s%s%s" quote x quote))
	(split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

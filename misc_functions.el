(defun peterc/chronos-open-another-buffer ()
  (interactive)
  (split-window-below)
  (save-excursion
    (other-window 1)
    (shrink-window 6)
    (switch-to-buffer "*chronos*")
    (other-window 1)))

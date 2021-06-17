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

(defun peterc/kill-done-column ()
  "Kill done column."
  (interactive)
  (while (re-search-forward "| d" nil t)
    (replace-match "| " ))
  )


(defun peterc/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;;; misc_functions.el ends here

(defun peterc/pin-this-window ()
  "Prevent this window from changing by other actions."
  (interactive)
 (set-window-dedicated-p (selected-window) t )
  )

;;; arc diff stuff
(defcustom laa-phab-arc "/opt/twitter_mde/bin/arc"
  "Phabricator ARC command."
  :type 'file
  :group 'laa)
(defun laa-phab-copy-to-paste (title lang)
  "Create a paste in phab with TITLE and LANG."
  (interactive "MTitle:\nMLanguage:")
  (let ((cmd (concat laa-phab-arc " paste --title=" title (when lang (concat " --lang=" lang)) " --")))
    (shell-command-on-region (region-beginning) (region-end) cmd)
    (setq deactivate-mark t)))
(defun laa-phab-get-paste (id)
  "Retrieve a paste by ID."
  (interactive "MId:")
  (shell-command (concat laa-phab-arc " paste " id " --")))

(defun peterc/toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

;; source https://stackoverflow.com/questions/15580913/is-there-a-way-to-toggle-a-string-between-single-and-double-quotes-in-emacs
(defun peterc/toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well.  Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (re-search-backward "[\"']")
    (let* ((start (point))
           (old-c (char-after start))
           new-c)
      (setq new-c 
            (case old-c
              (?\" "'")
              (?\' "\"")))
      (setq old-c (char-to-string old-c))
      (delete-char 1)
      (insert new-c)
      (re-search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (delete-char 1)
        (insert new-c)
        (replace-string new-c old-c nil (1+ start) end)))))

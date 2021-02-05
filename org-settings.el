;; org roam setup. a note taking system
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/Volumes/Private/notes/org-roam/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n r" . org-roam-db-build-cache))
              (("C-c n I" . org-roam-insert-immediate))
              (("C-c n d" . org-roam-dailies-map))))

;; install org roam server. a visualization of org roam
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8135 ;; unconventional port number to avoid colision
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


;; misc function
(defun org-time-stamp-inactive-with-time ()
  "function to do inactive time stamp with time. this avoid colision with evil's C-u settings"
  (interactive)
  (org-time-stamp-inactive (current-time))
  )

;; Org mode: special highlight for bold text
(setq org-emphasis-alist
  '(("*" (bold :foreground "Orange" ))
    ("/" italic)
    ("_" underline)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t))))

;; Org-mode: custom timestamp
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))


;; Org mode: Todo states
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "BLOCKED" "MERGED" "VERIFIED" "|" "DONE" "DELEGATED")))

;; Org mode: hide emphasis character like *bold*
(setq org-hide-emphasis-markers t)

;;  custom ord agenda view
;; source: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

;; Enable code block for dot, a diagram drawing mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

;; org mode custom backend to ignore links when export
(defun my-ox-jira-link (link desc info) desc)
(org-export-define-derived-backend 'my-jira 'jira
    :menu-entry
     '(?J "Export to Jira without links" (lambda (a s v b) (ox-jira-export-as-jira a s v)))
    :translate-alist '((link . my-ox-jira-link)))

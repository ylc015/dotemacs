;; add your TODO list to this variable so that it would show up on global TODO
(setq org-agenda-files (quote ("~/Dropbox/org/inbox.org" "~/Dropbox/org/privateJournal.org" "/Volumes/Private/notes/")))

(add-to-list 'load-path "~/.emacs.d/elpa/org-9.4/org-protocol.el")
(require 'org-protocol)
(require 'org-capture)
(require 'org-roam)

;; org reveal. a presentation framework
(setq org-reveal-root "file:///Users/peterc/perm_tmp/node_modules/reveal.js")

;; set archive location. mind the double colon at the end
(setq org-archive-location "/Volumes/Private/notes/archive.org::") 

;; SET org roam backend. allows for fuzzy search of file
(setq org-roam-completion-system 'ivy)

;; set org capture default file
(setq org-default-notes-file "~/Dropbox/org/inbox.org")
;; (require 'org-protocol)
 
;; function used by org-protocol chrome extension to santitize link
;; (defun transform-square-brackets-to-round-ones(string-to-transform)
;;   "Transforms [ into ( and ] into ), other chars left unchanged."
;;   (concat 
;;   (mapca
;;   r #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
;;   )

;; ORG capture templates
(setq org-capture-templates
      '(("t" "Work TODO Task"  entry (file org-default-notes-file)
	 "* TODO %?\n %U" :empty-lines 1)
	("T" "Work TODO with Clipboard" entry (file org-default-notes-file)
	    "* TODO %?\n%U\n   %c" :empty-lines 1)
	("K" "Knowledge with Clipboard" entry (file "/Volumes/Private/notes/knowledge.org") "* %?\n%U\n   %c" :empty-lines 1)
	("p" "Protocol" entry (file org-default-notes-file)
        "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file org-default-notes-file) 
        "* %? [[%:link][%:description]] \nCaptured On: %U")
    ))

;; org roam specific custom templates
;; see https://orgmode.org/manual/Template-expansion.html for more template expansion macros
(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
	"%?"
	:file-name "%<%Y%m%d%H%M%S>-${slug}"
	:head "#+title: ${title}\n#+ROAM_TAGS: %^{ROAM_TAGS|default}\n\n* definition\n  %^{definition}"
	:unnarrowed t)
	("a" "anki" plain (function org-roam--capture-get-point)
	"%?"
	:file-name "%<%Y%m%d%H%M%S>-${slug}"
	:head "#+title: ${title}\n#+CREATED: %T\n\n* definition :%^{TAGS}:\n :PROPERTIES:\n :ANKI_DECK: %^{ANKI_DECK}\n :ANKI_NOTE_TYPE: Basic \n :ANKI_TAGS: %\\1 \n :END:\n** Front\n ${title}\n** Back\n %^{definition}\n\n"
	:unnarrowed t)
	("c" "cfa" plain (function org-roam--capture-get-point)
	"%?"
	:file-name "%<%Y%m%d%H%M%S>-${slug}"
	:head "#+title: ${title}\n#+ROAM_TAGS: %^{ROAM_TAGS|CFA:Economics:Finance}\n\n* definition\n  %^{definition}"
	:unnarrowed t)
	("q" "cfa-question" plain (function org-roam--capture-get-point)
	"%?"
	:file-name "%<%Y%m%d%H%M%S>-${slug}"
	:head "#+title: ${title}\n#+ROAM_TAGS: %^{ROAM_TAGS|CFA:Economics:Finance}\n\n* definition\n  ${title} %^{definition}
* %^{Question|What is ${title}?} :Finance:CFA:
 :PROPERTIES:
 :ANKI_DECK: CFA
 :ANKI_NOTE_TYPE: Basic 
 :ANKI_TAGS: Finance:CFA 
 :END:
** Front
  %\\3
** Back
  ${title} %\\2
"
	:unnarrowed t)
	))


;; org roam setup. a note taking system
(use-package org-roam
      :ensure t
      :hook
      ((after-init . org-roam-mode)
       (org-mode . (lambda () evil-org-mode)))
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
;; remember to use a version that works with org-raom
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8136 ;; unconventional port number to avoid colision
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
;;(setq-default org-display-custom-times t)
;;(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))


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
 '((dot . t)
   (ledger . t) 
   (shell . t) 
   (python . t) 
   )) ; this line activates dot

;; org mode custom backend to ignore links when export
;;(defun my-ox-jira-link (link desc info) desc)
;;    (org-export-define-derived-backend 'my-jira 'jira
;;	:menu-entry
;;	'(?J "Export to Jira without links" (lambda (a s v b) (ox-jira-export-as-jira a s v)))
;;	:translate-alist '((link . my-ox-jira-link)))


;; Open agenda vertically by default
(defadvice org-agenda (around split-vertically activate)
  (let (
    (split-width-threshold 30)    ; or whatever width makes sense for you
    (split-height-threshold nil)) ; but never horizontally
ad-do-it))



;;; Commentary 

;; don't show the start up screen
(setq inhibit-startup-message t)

(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
	;;("MARMALADE" . "http://marmalade-repo.org/packages/")
	)
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
	;;("MARMALADE"    . 5)
        ("MELPA"        . 0)))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
 

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; make modeline changes color depending on evil state
(setq original-background (face-attribute 'mode-line :background))
(setq insert-state-background "#617cff")
(setq normal-state-background "#ff0000")
(setq emacs-state-background "#61ef4e")
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background normal-state-background)))
(add-hook 'evil-normal-state-exit-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background original-background)))
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background insert-state-background)))
(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background original-background)))
(add-hook 'evil-emacs-state-entry-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background emacs-state-background)))
(add-hook 'evil-emacs-state-exit-hook
          (lambda ()
            (set-face-attribute 'mode-line nil :background original-background)))


;; disable menu bar
( menu-bar-mode -1 )

;; custom function
(defun package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))


;; treat - and _ as part of the word in python mode
(require 'python)
(modify-syntax-entry ?_ "w" python-mode-syntax-table)
(modify-syntax-entry ?- "w" python-mode-syntax-table)
;; Python Hook
(add-hook 'python-mode-hook
(function (lambda ()
	    (setq indent-tabs-mode nil
		tab-width 2))))

(use-package spacemacs-theme
    :defer t
    :init (load-theme 'spacemacs-dark t))


;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Recentf enabled by default
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(require 'evil-mc)
(require 'evil-multiedit)

(require 'deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)


;; set bold text to be more obvious
(set-face-attribute 'bold nil :height 240)

;; turn on yas-wrap-around-region, it will inject value in $0 to the snipet
(setq yas-wrap-around-region t)

;; Enable yas-minor-mode only in org-mode
(add-hook 'org-mode-hook 'yas-minor-mode-on)
;; Enable evil org mode in org-mode
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;; Enable yas global ly
(yas-global-mode)
;; enable org bullet by default
(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Enable Company mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Enabled Yaml mode by default
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Enabled Python mode in aurora configs
(add-to-list 'auto-mode-alist '("\\.aurora\\'" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.workflow\\'" . json-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
;; emmet mode for html expansion
(require 'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . emmet-mode))

;; Code commenting
(use-package evil-nerd-commenter :ensure t)

;; Project management
(use-package
  projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))
(use-package counsel-projectile 
  :ensure t
  :config
  (counsel-projectile-mode))

;; Ivy & friends
(use-package ivy
  :ensure t)
(use-package counsel
  :ensure t)

;; Ranger
(use-package ranger 
  :ensure t
  :init
  (setq ranger-show-hidden t))

;; Workspaces
(use-package perspective
  :ensure t
  :config
  (persp-mode))
(use-package persp-projectile
  :ensure t)

;; jq-mode
(with-eval-after-load "json-mode"
  (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively))

;; Edit this config
(defun edit-emacs-configuration ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun edit-general-note ()
  (interactive)
  (find-file "/Volumes/Private/notes/general.org"))

(defun edit-journal-note ()
  (interactive)
  (find-file "~/Dropbox/org/inbox.org"))

(defun edit-private-journal-note ()
  (interactive)
  (find-file "~/Dropbox/org/privateJournal.org"))

(defun toggle-buffers ()
  (interactive)
  (switch-to-buffer nil))

;; Make evil undo more like vim
(setq evil-want-fine-undo t)

;; set global highlight current line
(global-hl-line-mode 1)

;; enable evil search to look for symbols
;; local buffer only
(setq evil-symbol-word-search t)

;; Keybindings
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state))
(use-package general
  :ensure t
  :config 
  (general-define-key
   "M-x" 'counsel-M-x)
  (general-define-key
   :states '(normal visual emacs)
   "/" 'swiper
   "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-define-key
   :states '(normal visual)
   "C-u" 'scroll-down-command
   "C-d" 'scroll-up-command)
  (general-define-key
   :states '(normal visual insert)
   "C-r" 'evil-undo)
  (general-define-key
   :states '(insert)
   "TAB" 'company-complete)
   ;; Enable table expansion in org-mode
  (general-define-key
   :states '(insert)
   :keymaps 'org-mode-map
   "TAB" 'org-cycle)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "'"   'multi-term
   "/"   'counsel-ag
   ":"   'counsel-M-x
   "."   'edit-emacs-configuration
   "TAB" 'toggle-buffers

   "p" 'projectile-command-map
   "pp" 'projectile-persp-switch-project
   "pf" 'counsel-projectile
   "pb" 'projectile-display-buffer

   "b" '(:ignore t :which-key "Buffers")
   "bb"  'ivy-switch-buffer

   "bl" '(:ignore t :which-key "Bloop")
   "bla"  'bloop-run-ammonite
   "bls"  'bloop-switch-to-ammonite
   "blc"  'bloop-compile
   "blt"  'bloop-test
   "blr"  'bloop-run

   "f" '(:ignore t :which-key "Files")
   "ff" 'counsel-fzf
   "fr" 'recentf-open-files
   "fl" 'revert-buffer ;; load file from disk

   "l" '(:ignore t :which-key "Lsp mode")
   ;; goto definition
   "lgd" 'lsp-ui-peek-find-definitions
   "lgr" 'lsp-ui-peek-find-references 
   "lr" 'lsp-rename
   "lc" 'lsp-ui-flycheck-list
   "lv" 'lsp-ui-flycheck-list--visit 
   "li" 'lsp-ui-imenu
   "la" 'lsp-execute-code-action

   "o" '(:ignore t :which-key "Org Mode")
   "oa" 'org-agenda
   "oi" 'org-time-stamp-inactive-with-time

   "e" '(:ignore t :which-key "Eval")
   "eb" 'eval-buffer
   "er" 'eval-region

   "w" '(:ignore t :which-key "Window")
   "wl"  'windmove-right
   "wh"  'windmove-left
   "wk"  'windmove-up
   "wj"  'windmove-down
   "w/"  'split-window-right
   "w-"  'split-window-below
   "wx"  'delete-window
   "wa"  'ace-window


   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'deer
   "ay" 'yas-insert-snippet

   "s" '(:ignore t :which-key "Search")
   "sc" 'evil-ex-nohighlight
   "sl" 'ivy-resume
   "sr" 'query-replace-regexp
   "sa" 'avy-goto-word-or-subword-1
   "sb" 'counsel-bookmark

   "t" '(:ignore t :which-key "Toggles")
   "tn" 'display-line-numbers-mode
   "tl" 'toggle-truncate-lines
   "tc" 'xclip-mode 

   "T" 'counsel-load-theme
   
   "x" '(:ignore t :which-key "Text")
   "xl" '(:ignore t :which-key "Lines")
   "xls" 'sort-lines
   
   "c" '(:ignore t :which-key "Code?")
   "cc" 'evilnc-comment-or-uncomment-lines
   "cm" 'evil-multiedit-match-all

   "g" '(:ignore t :which-key "Git")
   "gg" 'magit-status

   "n" '(:ignore t :which-key "Notes")
   "ng" 'edit-general-note
   "nj" 'edit-journal-note
   "np" 'edit-private-journal-note
   
   "nc" 'org-roam-dailies-capture-today
   "nt" 'org-roam-dailies-today
   ))

;; Custom files
(load-file "~/.emacs.d/scala-metals-init.el")
(load-file "~/.emacs.d/bloop.el")
(load-file "~/.emacs.d/misc_functions.el")
(load-file "~/.emacs.d/org-settings.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#dedede")
 '(line-spacing 0.2)
 '(org-agenda-files (quote ("~/Dropbox/org/inbox.org" "/Volumes/Private/notes/")))
 '(package-selected-packages
   (quote
    (ox-jira py-autopep8 python-black org-drill jq-mode chronos rust-mode evil-org deadgrep highlight-symbol org-roam-server org org-roam org-ql poet-theme evil-mc yasnippet-snippets evil-surround yaml-mode ammonite-term-repl company-lsp yasnippet lsp-ui lsp-metals lsp-mode flycheck sbt-mode scala-mode ranger persp-projectile counsel-projectile projectile butler jenkins undo-fu undo-tree swiper-helm counsel spacemacs-theme magit use-package)))
 '(xclip-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

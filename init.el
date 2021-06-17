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

;; jira settings
(setq jiralib-url "https://jira.twitter.biz")
 

(add-to-list 'load-path "~/.emacs.d/evil")
;; special tunning for evil collection. which enable evil binding in special mode like help page and edebug
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

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


;; Use work python
(setq python-python-command "/usr/local/bin/twpython3")
(setq python-shell-interpreter "/usr/local/bin/twpython3")

;; treat - and _ as part of the word in python mode
(require 'python)
(modify-syntax-entry ?_ "w" python-mode-syntax-table)
(modify-syntax-entry ?- "w" python-mode-syntax-table)
;; Python Hook
(add-hook 'python-mode-hook
(function (lambda ()
	    (setq indent-tabs-mode nil
		tab-width 2))))
(add-hook 'python-mode-hook
	  '(lambda () 
		(setq python-indent 2)
		(setq evil-shift-width 2)
		))


;; Use my own flake8 binary
(setq flycheck-python-flake8-executable "/Users/peterc/.local/bin/flake8")

(use-package spacemacs-theme
    :defer t
    :init (load-theme 'spacemacs-dark t))

;; Plantuml ( a UML diagram creator mode )
;; see for more info https://github.com/skuro/plantuml-mode
(setq plantuml-jar-path "/Users/peterc/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type 'ascii)


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

;; undo region enabled!!! select a region and undo from there
(setq undo-tree-enable-undo-in-region t)

;; set bold text to be more obvious. becareful. this mess with headers too
(set-face-attribute 'bold nil :height 170)
(set-face-attribute 'default nil :height 150)

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
(add-to-list 'auto-mode-alist '("\\.aurora\\'" . aurora-config-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))


;; pants support for emacs https://github.com/fcuny/pants.el
;; for some reason load-path is not working as expected
;; we have to manually load pants.el here
(load-file "~/.emacs.d/pants.el/pants.el")
(use-package pants
  :load-path "pants.el/pants.el"
  :bind (("C-c b" . pants-find-build-file)
         ("C-c r" . pants-run-run)
         ("C-c x" . pants-run-binary)
         ("C-c t" . pants-run-test)
         ("C-c f" . pants-run-fmt)
         ("C-c l" . pants-run-lint)
         ("C-c m" . pants-run-mypy)
         ("C-c k" . kill-compilation)
	 )
  :mode (("BUILD\\'" . pants-build-mode))
  :custom
  (pants-source-tree-root "/Users/peterc/workspace/source")
  (pants-bury-compilation-buffer t)
  (pants-extra-args "-q")
  (pants-completion-system 'ivy))

;; make compilation buffer follows output  
(setq compilation-scroll-output t)
;;(setq compilation-scroll-output 'first-error)

;; Flycheck integration with mypy. A static type checker in python 
(require 'flycheck)
(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-pylint 'python-mypy t)

;; Sh mode hook
(add-to-list 'auto-mode-alist '(".bash_alias" . sh-mode))

;; Enable lispyville whenever lispy mode is on
(add-hook 'lispy-mode-hook #'lispyville-mode)

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
;; replace all minibuffer completing with ivy-mode
(ivy-mode 1)

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

(defun edit-knowlege-note ()
  (interactive)
  (find-file "/Volumes/Private/notes/knowledge.org"))

(defun open-my-journal-workplace ()
  ;; opens inbox.org and open agenda view by default
  (interactive)
  (edit-journal-note)
  (call-custom-agenda-view)
  (other-window 1)
  )

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

(defun call-custom-agenda-view ()
  ;; Call direct custom agenda view
  ;; see org-setting.el for the custom view
  (interactive)
  (org-agenda nil "d"))

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
   :states '(normal insert visual)
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
   "oa" 'call-custom-agenda-view
   "oi" 'org-time-stamp-inactive-with-time
   "oc" 'org-capture
   "od" 'org-roam-dailies-today
   "or" 'org-roam-db-build-cache

   ;; beware that ideally these binding should be left hand only
   "a" '(:ignore t :which-key "Artist Mode")
   "aa" 'artist-mouse-choose-operation
   
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

   "l" '(:ignore t :which-key "link")
   "ll" 'goto-address-at-point

   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ad" 'deer
   "ay" 'yas-insert-snippet
   "ak" 'anki-editor-push-notes
   "ac" 'anki-editor-cloze-region

   "s" '(:ignore t :which-key "Search")
   "sc" 'evil-ex-nohighlight
   "sl" 'ivy-resume
   "sr" 'query-replace-regexp
   "sa" 'avy-goto-word-or-subword-1
   "sb" 'counsel-bookmark

   "t" '(:ignore t :which-key "Toggles")
   "tn" 'display-line-numbers-mode
   "tr" (lambda () (interactive) (display-line-numbers-mode 'relative))
   "tl" 'toggle-truncate-lines
   "tc" 'xclip-mode 
   "tol" 'org-toggle-link-display

   "T" 'counsel-load-theme
   
   "x" '(:ignore t :which-key "Text")
   "xl" '(:ignore t :which-key "Lines")
   "xls" 'sort-lines
   
   "c" '(:ignore t :which-key "Code?")
   "cc" 'evilnc-comment-or-uncomment-lines
   "cm" 'evil-multiedit-match-all
   "cn" 'flycheck-next-error
   "cj" 'jq-format-json-buffer
   

   "g" '(:ignore t :which-key "Git")
   "gg" 'magit-status

   "n" '(:ignore t :which-key "Notes")
   "ng" 'edit-general-note
   ;;"nj" 'edit-journal-note
   "nj" 'open-my-journal-workplace
   "np" 'edit-private-journal-note
   "nk" 'edit-knowlege-note
   
   "nc" 'org-roam-dailies-capture-today
   "nt" 'org-roam-dailies-today
   ))

;; use the PATH variable from bash terminal
;; not sure why but if this is put at the begin it will error
(exec-path-from-shell-initialize)

;; Custom files
(load-file "~/.emacs.d/scala-metals-init.el")
(load-file "~/.emacs.d/bloop.el")
(load-file "~/.emacs.d/misc_functions.el")
(load-file "~/.emacs.d/org-settings.el")
(load-file "~/.emacs.d/alfred-org-capture.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fci-rule-color "#dedede")
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-jscs javascript-standard json-jsonlint json-python-json less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 r-lintr racket rpm-rpmlint markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby python-mypy)))
 '(line-spacing 0.2)
 '(org-agenda-files
   (quote
    ("/Volumes/Private/notes/org-roam/daily/2021-03-31.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/privateJournal.org" "/Volumes/Private/notes/general.bk.org" "/Volumes/Private/notes/general.org" "/Volumes/Private/notes/investing.org" "/Volumes/Private/notes/journal.org" "/Volumes/Private/notes/knowledge.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-mode-hook
   (quote
    (#[0 "\300\301\302\303\304$\207"
	 [add-hook change-major-mode-hook org-show-all append local]
	 5]
     #[0 "\300\301\302\303\304$\207"
	 [add-hook change-major-mode-hook org-babel-show-result-all append local]
	 5]
     org-babel-result-hide-spec org-babel-hide-all-hashes
     #[0 "\301\211\207"
	 [imenu-create-index-function org-imenu-get-tree]
	 2]
     (lambda nil evil-org-mode)
     org-bullets-mode yas-minor-mode-on evil-org-mode)))
 '(package-selected-packages
   (quote
    (org-table-sticky-header anki-editor tea-time ggtags eglot org-download hledger-mode plantuml-mode ledger-mode exec-path-from-shell ox-slack exwm pamparam org-drill-table yapfify org-noter neotree pdf-tools stripes md4rd vterm ox-reveal org-re-reveal hackernews org-jira evil-collection ox-jira py-autopep8 python-black org-drill jq-mode chronos rust-mode evil-org deadgrep highlight-symbol org-roam-server org org-roam org-ql poet-theme evil-mc yasnippet-snippets evil-surround yaml-mode ammonite-term-repl company-lsp yasnippet lsp-ui lsp-metals lsp-mode flycheck sbt-mode scala-mode ranger persp-projectile counsel-projectile projectile butler jenkins undo-fu undo-tree swiper-helm counsel spacemacs-theme magit use-package)))
 '(show-paren-mode t)
 '(xclip-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#212026")))))
(put 'upcase-region 'disabled nil)

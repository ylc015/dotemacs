;;; bloop --- bloop minor mode

;; Author: Philipp Fehre <philipp@fehre.co.uk>
;; Keywords: scala, bloop, tools, convenience

;;; Commentary:
;; Helpers to integrate better with bloop, inspired by emacs-bloop
;; https://github.com/tues/emacs-bloop/blob/master/bloop.el

;; C-c M-j jack-in a bloop project running a new Ammonite REPL buffer
;; C-c M-z switch to an active Ammonite REPL
;; C-c b c Compile a bloop project backed by projectile-compile-project
;; C-c b t Test a bloop project backed by projectile-test-project
;; C-c b r Run a bloop project

;; Changelog:
;; - 25/8/2020 - Added run command mapping bloop-run (C-c b r)
;; - 1/5/2020 - initial working version

;;; Code:

(require 'projectile)
(require 'scala-mode)
(require 'ammonite-term-repl)
(require 's)

(defgroup bloop nil
  "Bloop integration for emacs"
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Gist" "https://gist.github.com/sideshowcoder/a9b2ceaca38cdf0ea95f29bf0130b171"))

(defcustom bloop-program-name "bloop"
  "Program used to run bloop commands, default to whatever is in the path."
  :type 'string
  :group 'bloop)

(defcustom bloop-reporter "scalac"
  "Either bloop or scalac.

The main difference is that bloop shows errors in reverse order.
Emacs generally assumes the first error in the output is the most
relevant so the scalac reporter will most likely be preferred.
This is used for test and compile."
  :type 'string
  :group 'bloop)

(defun bloop--command (&rest args)
  "Build a bloop command for ARGS."
  (s-join " " (cons bloop-program-name args)))

(defun bloop--available-projects ()
  "Get a list of currently available projects from bloop."
  (projectile-with-default-dir (projectile-project-root)
    (let ((projects-string (shell-command-to-string (bloop--command "projects"))))
      (split-string projects-string))))

(defun bloop-switch-to-ammonite ()
  "Switch to the running Ammonite REPL."
  (interactive)
  (if-let ((ammonite-buffer (get-buffer ammonite-term-repl-buffer-name)))
      (switch-to-buffer ammonite-buffer)
    (message "Ammonite is not running try C-c M-j to start an Ammonite REPL for bloop.")))

(defun bloop-run-ammonite (project)
  "Run Ammonite for a bloop PROJECT."
  (interactive (list (completing-read "Run Ammonite REPL for project: " (bloop--available-projects))))
  (projectile-with-default-dir (projectile-project-root)
    (let ((ammonite-term-repl-program bloop-program-name)
          (ammonite-term-repl-program-args (list "console" project)))
      (run-ammonite))))

(defun bloop-compile (project)
  "Compile a bloop PROJECT."
  (interactive (list (completing-read "Compile bloop project: " (bloop--available-projects))))
  (let ((command (bloop--command "compile" project)))
    (projectile--run-project-cmd command projectile-compilation-cmd-map
                                 :show-prompt 't
                                 :prompt-prefix "Compile command: "
                                 :save-buffers t)))


(defun bloop-test (project)
  "Test a bloop PROJECT."
  (interactive (list (completing-read "Test bloop project: " (bloop--available-projects))))
  (let ((test-command (bloop--command "test" "--reporter" bloop-reporter project)))
    (projectile--run-project-cmd test-command projectile-test-cmd-map
                                 :show-prompt 't
                                 :prompt-prefix "Test command: "
                                 :save-buffers t)))

(defun bloop-run (project)
  "Run a bloop PROJECT."
  (interactive (list (completing-read "Run bloop project: " (bloop--available-projects))))
  (let ((run-command (bloop--command "run" project)))
    (projectile--run-project-cmd run-command projectile-run-cmd-map
                                 :show-prompt 't
                                 :prompt-prefix "Run command: "
                                 :save-buffers t)))

;;;###autoload
(define-minor-mode bloop-mode
  "Bloop integration for emacs."
  :lighter " bloop"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c M-j") #'bloop-run-ammonite)
            (define-key map (kbd "C-c M-z") #'bloop-switch-to-ammonite)
            (define-key map (kbd "C-c b c") #'bloop-compile)
            (define-key map (kbd "C-c b t") #'bloop-test)
            (define-key map (kbd "C-c b r") #'bloop-run)
            map))

;;;###autoload
(add-hook 'scala-mode-hook 'bloop-mode)

(provide 'bloop)
;;; bloop.el ends here

;;;; system-windowsnt.el -- windows-specific config

;; firefox.exe must be in the PATH
(if (string-equal system-type "windows-nt")
    (setq browse-url-firefox-program (executable-find "firefox.exe")))

;;; Dictionary completion ----------------------------------------------

(setq ispell-complete-word-dict "C:/Program Files/words/american.2")
(setq ispell-grep-command "C:/cygwin/bin/grep")


;;; Cygwin as our shell -----------------------------------------------

(let* ((cygwin-root "c:/cygwin64")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (file-readable-p cygwin-root)
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))

    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)

    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;;; TODO: setup a cmd shell w/ ORIGINAL env (PATH) ---------------------

;; (defun cmdexe-shell ()
;;   "Run cmd.exe as our shell"
;;   (interactive)
;;   (let ((process-environment orig-env)
;;         (explicit-shell-file-name "cmd.exe"))
;;     (shell))))

;;;; host-kelma12755.el -- host-specific config

(require 'mk-project)
(require 'cl)

;;; Javadoc-help --------------------------------------------------------

(require 'javadoc-help)

;; What an ugly hack this is! This pkg needs some love.
(let ((refreshed t)
      (enabled t)
      (predefined t))
  (setq *jdh-javadocs* (mapcar (lambda (url) (jdh-javadoc-new url refreshed enabled predefined))
                               '("c:/Documents and Settings/kelma12/My Documents/apidocs/jdk1.6.0-api"
                                 "c:/Program Files/Java/Glassfish/glassfish/docs/api"))))

;;; --------------------------------------------------------------------
;;; SOA SM Views
;;; --------------------------------------------------------------------

(defvar soasm-ps-build-view "m:\\rel-sm-r12-sp2-win32")

(defun soasm-project-def (name static-p)
  (let* ((bd (if static-p (concat "C:/cc-views/" name "/devel/")
                          (concat "M:/" name "/devel/")))
         (pd (concat "~/mk-project/" name "/")))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (and static-p (not (file-readable-p bd)))
        (message "Warning: static clearcase view %s does not exist" bd))
    (message "Defining soasm project %s at basedir %s" name bd)
    (project-def name
                 `((basedir ,bd)
                   (src-patterns ("*.java$" "*.cpp" "*.[cChH]"))
                   (ignore-patterns ("*.wsdl" "*.class" "*.obj" "*.o" ".so"))
                   (src-find-cmd   soasm-find-cmd)
                   (grep-find-cmd  soasm-find-cmd)
                   (index-find-cmd soasm-find-cmd)
                   (tags-file ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache ,(concat pd "file-list-cache"))
                   (ack-args "--java")
                   (startup-hook soasm-startup)
                   (shutdown-hook soasm-shutdown)))))

(defun soasm-startup ()
  (global-set-key (kbd "C-c p c") 'write-soasm-build-script))

(defun soasm-shutdown ()
  (global-set-key (kbd "C-c p c") 'project-compile))


(defun soasm-find-cmd (context)
  (assert mk-proj-basedir)
  (let* ((dev-dir (concat mk-proj-basedir ""))
         (not-dev-dir (concat dev-dir "/thirdparty/nsldap/6.0/SunOS/lib/secv1"))
         (base-find (concat "find '" dev-dir "'"))
         (pruner    (concat " \\( -path '" not-dev-dir "' -prune \\) "))
         (src-find " \\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' -o -name '*.java' \\) -print \\)")
         (more-find " \\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' -o -name '*.java' -o -name 'Makefile.*' \\) -print \\)"))
    (ecase context
      ('src   (concat base-find pruner " -o " src-find))
      ('grep  (replace-regexp-in-string "print" "print0" (concat "find . " pruner " -o " more-find)))
      ('index (concat base-find pruner "-o -print")))))

(when (string-equal system-type "windows-nt")
  (mapcar (lambda (n) (soasm-project-def n nil)) '("kelma12-r12.5" "kelma12-r12.5-fed-int"))
  (mapcar (lambda (n) (soasm-project-def n t)) '("kelma12-r12.5-soa-svc")))

;;; --------------------------------------------------------------------
;;; SOASM Utils
;;; --------------------------------------------------------------------

(defun w32-path (path)
  (let ((newpath (shell-command-to-string (concat "cygpath -w '" path "'"))))
    (substring newpath 0 (- (length newpath) 1)))) ; chomp

;; TODO: should this write a Makefile instead of a script?
(defun write-soasm-build-script (shortcut-p clean-p)
  "Write a .bat file to compile the current SOA view."
  (interactive "xShortcut build? \nxClean build? ")
  (mk-proj-assert-proj)
  (let* ((build-script (expand-file-name "~/soa-build.bat"))
         (view-root-dir (w32-path (replace-regexp-in-string "/devel/" "" mk-proj-basedir)))
         (devel-dir (w32-path mk-proj-basedir))
         (go-home (concat "cd " devel-dir "\n"))
         (static-view-p (string-match "cc-views" view-root-dir))) ; unused
    ;; write our build script to disk
    (with-temp-file build-script
      (insert "echo on\n")
      (insert (concat
               "call set PS_BUILD_VIEW=" (if shortcut-p soasm-ps-build-view view-root-dir) "\n"
               "call set PS_VIEW_PREFIX=%PS_BUILD_VIEW%\\devel\n"
               "call set WA_BUILD_VIEW=" (if shortcut-p soasm-ps-build-view view-root-dir) "\n"
               "call set WA_VIEW_PREFIX=%WA_BUILD_VIEW%\\devel\n"
               ;; TM_* unused
               ;; "call set TM_BUILD_VIEW=" ps-build-view "\n"
               ;; "call set TM_VIEW_PREFIX=%TM_BUILD_VIEW%\\devel\n"
               "\n"))
      (insert go-home)
      (insert (concat
               "cd tools\\bin\n"
               "call buildenv\n\n"))
      (insert go-home)
      (unless shortcut-p
        (dolist (d '("common" "sdk" "xps" "policy-server" "agentcommon"
                     "agentframework" "affl-minder\\fedcommon"
                     "affl-minder\\policy-server"))
          (insert (concat "cd " d "\n"))
          (when clean-p (insert "call clearmake -VF clean\n"))
          (insert "call clearmake -VF\n")
          (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")
          (insert go-home)))
      (dolist (d '("transactionminder"))
        (insert (concat "cd " d "\n"))
        (when clean-p (insert "call clearmake -VF clean\n"))
        (insert "call clearmake -VF\n")
        (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")
        (insert go-home))
      (dolist (d '("app-server\\was\\scripts"))
        (insert (concat "cd " d "\n"))
        (insert "call build-all.cmd\n")
        (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")
        (insert go-home))
      (dolist (d '("app-server\\sspi\\scripts"))
        (insert (concat "cd " d "\n"))
        (when (insert "call build-all.cmd clean\n"))
        (insert "call build-all.cmd\n")
        (insert "ECHO Oh no! ERRORLEVEL isn't set correctly!\n")
        (insert "ECHO IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")
        (insert go-home))
      (dolist (d '("soaagent" "soamgr"))
        (insert (concat "cd " d "\n"))
        (when (insert "call clearmake -VF clean\n"))
        (insert "call clearmake -VF\n")
        (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")
        (insert go-home))
      (insert "\necho Compile OK\nGOTO DONE\n")
      (insert ":ERROR\necho Compile FAILED\n")
      (insert ":DONE\n"))
    (assert (file-readable-p build-script))

    ;; ;; run the build script in compile mode
    ;; (let* ((default-directory (file-name-directory build-script))
    ;;        (bat-script (file-name-nondirectory build-script)))
    ;; (compile (concat "cmd /c " bat-script) t))))
    ))

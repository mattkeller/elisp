;;;; host-kelma12755.el -- host-specific config

(require 'mk-project)
(require 'cl)
;(require 'clearcase)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized-dark)
(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-9")

(add-to-list 'auto-mode-alist '("Makefile\\..*$" . makefile-mode))

(javadoc-add-roots "c:/Documents and Settings/kelma12/My Documents/apidocs/jdk1.6.0-api"
                   "c:/Program Files/Java/Glassfish/glassfish/docs/api")

(defun java-env-setup ()
  (interactive)
  (setenv "PATH" (concat "/cygdrive/c/Program Files/Java/jdk1.6.0_31/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "/cygdrive/c/Program Files/ant-1.8.3/bin:" (getenv "PATH")))
  (setenv "JAVA_HOME" "C:/Program Files/Java/jdk1.6.0_31")
  (setenv "ANT_HOME" "C:/Program Files/ant-1.8.3"))

(defun devel-shell ()
  (interactive)
  (let* ((path '("/cygdrive/c/Documents and Settings/kelma12/bin"
                 "/cygdrive/c/Program Files/ant-1.8.3/bin"
                 "/cygdrive/c/Program Files/Java/jdk1.6.0_31/bin"
                 "/cygdrive/c/Program Files/apache-maven-2.2.1/bin/cygwin/bin"
                 "/cygdrive/c/Program Files/clojure/lein"
                 "/cygdrive/c/Program Files/Ruby/bin"
                 "/cygdrive/c/cygwin/bin"
                 "/cygdrive/c/Program Files/Rational/common"
                 "/cygdrive/c/Program Files/Rational/ClearCase/bin"
                 "/cygdrive/c/Program Files/CA/SC/CAWIN/"
                 "/cygdrive/c/Program Files/CA/DSM/bin"
                 "/cygdrive/c/Program Files/CA/SC/CAM/bin"
                 "/cygdrive/c/WINDOWS/system32"
                 "/cygdrive/c/WINDOWS"
                 "/cygdrive/c/WINDOWS/System32/Wbem"
                 "//sol/cc-tools/bin"
                 "//sol/cc-tools/bin"))
         (path-str (mapconcat (lambda (s) s) path ":"))
         (buf (mk-shell-dwim t)))
    (with-current-buffer buf
      (insert (concat "export PATH=\"" path-str "\"\n"))
      (insert (concat "export JAVA_HOME=\"C:/Program Files/Java/jdk1.6.0_31\"\n"))
      (insert (concat "export ANT_HOME=\"C:/Program Files/ant-1.8.3\"\n")))))

;;; --------------------------------------------------------------------
;;; SOA SM Views
;;; --------------------------------------------------------------------

(defvar soasm-121-ps-build-view "m:\\soa-sm-r12-maint-110330")
(defvar soasm-125-ps-build-view "m:\\rel-sm-r12-sp2-win32")

(defun project-meta-dir (proj-name)
  (concat "~/mk-project/" proj-name "/"))

(defun soasm-project-def (name dir)
  (let* ((bd (file-name-as-directory dir))
         (pd (project-meta-dir name)))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (not (file-readable-p bd))
        (message "Warning: view %s does not exist" bd))
    (message "Defining soasm project %s at basedir %s" name bd)
    (project-def name
                 `((basedir          ,bd)
                   (src-patterns     ("*.java$" "*.cpp" "*.[cChH]"))
                   (ignore-patterns  ("*.wsdl" "*.class" "*.obj" "*.o" ".so"))
                   (src-find-cmd     soasm-find-cmd)
                   (grep-find-cmd    soasm-find-cmd)
                   (index-find-cmd   sm-index-files-cmd)
                   ;(tags-file        ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache  ,(concat pd "file-list-cache"))
                   (ack-args         "--java")
                   (compile-cmd      write-soasm-build-script)
                   (startup-hook     soasm-startup)
                   (shutdown-hook    soasm-shutdown)))))

(defun sm-project-def (name dir)
  (let* ((bd (file-name-as-directory dir))
         (pd (project-meta-dir name)))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (not (file-readable-p bd))
        (message "Warning: view %s does not exist" bd))
    (message "Defining sm project %s at basedir %s" name bd)
    (project-def name
                 `((basedir          ,bd)
                   (src-patterns     ("*.java$" "*.cpp" "*.[cChH]"))
                   (ignore-patterns  ("*.wsdl" "*.class" "*.obj" "*.o" ".so"))
                   (src-find-cmd     sm-find-cmd2)
                   (grep-find-cmd    sm-find-cmd2)
                   (index-find-cmd   sm-index-files-cmd)
                   (tags-file        ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache  ,(concat pd "file-list-cache") nil)
                   (ack-args         "--java")
                   (startup-hook     nil)
                   (compile-cmd      write-soasm-build-script)))))

(defun soasm-startup ()
  (mktags/load-project-tags)
  (message "Welcome to SOA"))

(defun soasm-shutdown ()
  (mktags/clear-tags)
  (message "Goodbye SOA"))

(defun sts-ant-build ()
  (interactive)
  (let ((mk-proj-compile-cmd '(ant-call "compile"))
        (current-directory (concat mk-proj-basedir "/devel/soamgr/soaservices/sts/")))
    (project-compile null)))

(defun soasm-find-cmd (context)
  "Generate a find command appropriate for the context (src, index, grep)"
  (assert mk-proj-basedir)
  (let* ((dev-dir     (concat mk-proj-basedir ""))
         (3p-dir      (concat dev-dir "thirdparty"))
         (foobar-dir  (concat 3p-dir "/nsldap/6.0/SunOS/lib"))
         (base-find   (concat "find '" dev-dir "'"))
         (prunefoobar (concat " \\( -path '" foobar-dir "' -prune \\) "))
         (prunegit    (concat " \\( -path '" (concat mk-proj-basedir ".git") "' -prune \\) "))
         (prune3p     (concat " \\( -path '" 3p-dir "' -prune \\) "))
         (src-find    (concat " \\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' "
                              "-o -name '*.java' \\) -print \\)"))
         (java-find   " \\( -type f \\( -name '*.java' \\) -print \\)")
         (more-find   (concat " \\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' "
                              "-o -name '*.java' -o -name 'Makefile.*' \\) -print \\)")))
    (ecase context
      ('src   (concat base-find prunefoobar " -o " src-find))
      ('index (concat base-find prunefoobar "-o" prunegit "-o -not -name '*.class$' -not -name '*.o$' -type f"))
      ('grep  (replace-regexp-in-string
               "print"
               "print0"
               (concat "find . "
                       (if (y-or-n-p "3rd Party? ") prunefoobar prune3p)
                       " -o "
                       (if (y-or-n-p "Java only? ") java-find more-find)))))))

(defun find-files-cmd (dir)
  (concat "find " dir " -type f"))

(defvar src-globs '("[cC]" "cpp" "[hH]" "java"))

(defun find-src-cmd (dir)
  (concat "find " dir " -type f "
          (mapconcat (lambda (ext)
                       (concat "-name '*." ext "'"))
                     src-globs
                     " -o ")))

(defun src-grep-cmd ()
  (concat "grep -e "
          (mapconcat (lambda (ext)
                       (concat "'." ext "$'"))
                     src-globs
                     " -e ")))

;; exclude thirdparty for now
(defvar sm-dirs '("admin-ui" "affl-minder" "agentcommon" "agentframework" "app-server" "aps"
                 "common" "dms" "epm" "erpconn" "federation" "ims" "jacc" "kikb" "policy-server"
                 "sdk" "web-agent" "webadmin" "xps" "xps-objects"))

(defun find-sm-srcs (basedir)
  (concat "\\("
          (mapconcat (lambda (dir) (concat (find-src-cmd dir)))
                     sm-dirs
                     " && ")
          "\\)"))

;; exclude thirdparty for now
(defun find-sm-files (basedir)
  (concat "\\("
          (mapconcat (lambda (dir) (concat (find-files-cmd dir)))
                     sm-dirs
                     "; ")
          "\\)"))

(defun sm-find-cmd (context)
  "Generate file lists for use by mk-project. Optimization: As
sources is a subset of the indexed files, grep the indexed file
list for source patterns instead of running find again"
  (assert mk-proj-basedir)
  (let* ((dev-dir (concat mk-proj-basedir ""))
         (3p-dir      (concat dev-dir "thirdparty"))
         (foobar-dir  (concat 3p-dir "/nsldap/6.0/SunOS/lib"))
         (base-find   (concat "find '" dev-dir "'"))
         (prunefoobar (concat " \\( -path '" foobar-dir "' -prune \\) "))
         (prunegit    (concat " \\( -path '" (concat mk-proj-basedir ".git") "' -prune \\) "))
         (prune3p     (concat " \\( -path '" 3p-dir "' -prune \\) "))
         (java-find   " \\( -type f \\( -name '*.java' \\) -print \\)")
         (more-find   (concat " \\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' "
                              "-o -name '*.java' -o -name 'Makefile.*' \\) -print \\)"))
         (index-file (concat (project-meta-dir mk-proj-name) "file-list-cache")))
    (ecase context
      ('src   (if (file-exists-p index-file)
                (concat "cat " index-file " | " (src-grep-cmd))
                (find-sm-srcs dev-dir)))
      ('index (find-sm-files dev-dir))
      ('grep  (replace-regexp-in-string
               "print"
               "print0"
               (concat "find . "
                       (if (y-or-n-p "3rd Party? ") prunefoobar prune3p)
                       " -o "
                       (if (y-or-n-p "Java only? ") java-find more-find)))))))

(require 'find)

;; TODO: experimental.
;; TODO: print0
(defun sm-find-cmd2 (context)
  "Generate file lists for use by mk-project."
  (assert mk-proj-basedir)
  (let ((default-directory mk-proj-basedir))
    (ecase context
      ('src   (find (prune (path "devel/thirdparty/nsldap/6.0/SunOS/lib"))
                    (prune (name ".git"))
                    (and (type "f")
                         (name ".[cChH]" ".cpp" ".java"))))
      ('index (find (prune (path "devel/thirdparty/nsldap/6.0/SunOS/lib"))
                    (prune (name ".git"))
                    (and (type "f")
                         (not (name ".o" ".class")))))
      ('grep  (let* ((threep (y-or-n-p "3rd Party? "))
                     (javap  (y-or-n-p "Java Only? "))
                     (cmd `(find (prune (name ".git"))
                                 ,(if threep
                                      '(prune (path "devel/thirdparty/nsldap/6.0/SunOS/lib"))
                                    '(prune (path "devel/thirdparty")))
                                 (and (type "f")
                                      ,(if javap
                                           '(name ".java")
                                         '(name ".[cChH]" ".cpp" ".java"))))))
                (eval cmd))))))

(defun sm-index-files-cmd (context)
  "Find files to index"
  (assert mk-proj-basedir)
  (let ((default-directory mk-proj-basedir)
        (dirs '("common" "sdk" "xps" "policy-server" "agentcommon" "affl-minder" "xps-objects"
                "federation" "transactionminder" "soaagent" "tools"))
        (cmd "find "))
    (dolist (d dirs)
      (when (file-exists-p  d)
        (setq cmd (concat cmd " " d))))
    (concat cmd " -name '*.java' -o -name '*.[cChH]' -o -name '*.cpp' -o -name 'Makfile*' "
            "-o -name '*.xml' -o -name '*.properties' -o -name '*.txt'")))

(defun create-soasm-projects ()
  (when (string-equal system-type "windows-nt")
    (mapcar (lambda (n) (soasm-project-def (car n) (cadr n)))
            '(("kelma12-soasm-r12.1-GA-317"         "M:/kelma12-soasm-r12.1-GA-317/devel")
              ("kelma12-soasm-r12.1sp3-toyota"      "M:/kelma12-soasm-r12.1sp3-toyota/devel")
              ("kelma12-soasm-r12.1sp3-toyota-nin"  "M:/kelma12-soasm-r12.1sp3-toyota-nin/devel")
              ("kelma12-soasm-r12.1sp3-staging"     "c:/cc-views/kelma12-soasm-r12.1sp3-staging/devel")
              ("kelma12-soasm-r12.1sp3-sigwrap"     "M:/kelma12-soasm-r12.1sp3-sigwrap/devel")
              ("kelma12-soasm-r12.1sp3-ant"         "M:/kelma12-soasm-r12.1sp3-ant/devel")
              ("bearer-sp3"                         "M:/kelma12-soasm-r12.1sp3-bearer-tmp/devel")
              ("kelma12-soasm-r12.1sp3-integration" "M:/kelma12-soasm-r12.1sp3-integration/devel")
              ("kelma12-r12.5"                      "M:/kelma12-r12.5/devel")
              ("kelma12-r12.5-soa-svc"              "C:/cc-views/kelma12-r12.5-soa-svc/devel")
              ("levmy01-soasvc12.51"                "K:/devel")))))

(create-soasm-projects)

(defun create-sm-projects ()
  (when (string-equal system-type "windows-nt")
    (mapcar (lambda (n) (sm-project-def (car n) (cadr n)))
            '(("kelma12-sm-r12-sp3-cr03-ca.fed-nin" "M:/kelma12-sm-r12-sp3-cr03-ca.fed-nin/devel")
              ("kelma12-r12.5-bugfix7-win"    "M:/kelma12-r12.5-bugfix7-win/devel")
              ("kelma12-r12.5-fedmgr-versioning"    "V:/devel")
              ("kelma12-r12.5-bugfix7-144415"       "M:/kelma12-r12.5-bugfix7-14415/devel")
              ("kelma12-r12.5-siteminder-121379"    "M:/kelma12-r12.5-siteminder-121379-win/devel")
              ("kelma12-cm1.1-fb04-sts-sigwrap"     "M:/kelma12-cm1.1-fb04-sts-sigwrap/devel")
              ("kelma12-cm1.1-fb04-sts-multi-app"   "M:/kelma12-cm1.1-fb04-sts-b2/devel")
              ("kelma12-cm1.1-fb04-sts-cert"        "M:/kelma12-cm1.1-fb04-sts-cert/devel")
              ("kelma12-cm1.1-fb04-sts-datamodels"  "M:/kelma12-cm1.1-fb04-sts-datamodel/devel")
              ("kelma12-cm1.1-fb01-infra-stsbugs1"  "M:/kelma12-cm1.1-fb01-infra-stsbugs1/devel")
              ("kelma12-cm1.1-fb05-sp1-sts3"        "M:/kelma12-cm1.1-fb05-sp1-sts3/devel")
              ("kelma12-r12.51-wss-tunnel-services" "M:/kelma12-r12.51-wss-tunnel-services/devel")
              ("kelma12-r12.5-fedmgr-integ"         "M:/kelma12-r12.5-fedmgr-integ/devel")))))

(create-sm-projects)

(defun create-soasm-project-dyn (dir)
  (interactive "DDirectory: ")
  (let ((name (read-string "Project name: " dir)))
    (soasm-project-def name (concat dir "/devel/"))))

(let ((pd (concat (getenv "HOME") "Desktop\\sts.git")))
(project-def "sts.git"
  `((basedir          ,pd)
    (src-patterns     ("*.java$"))
    (ignore-patterns  ("*.wsdl" "*.class"))
    (tags-file        ,(concat pd "TAGS"))
    (open-files-cache ,(concat pd ".mk-project-open-files"))
    (file-list-cache  ,(concat pd "file-list-cache") nil)
    (ack-args         "--java")
    (compile-cmd      write-soasm-build-script)
    (startup-hook     soasm-startup)
    (shutdown-hook    soasm-shutdown))))

(let ((pd "C:/cc-views/txm-experimental/devel/"))
  (project-def "txm-experimental"
               `((basedir          ,pd)
                 (src-patterns     ("*.java$"))
                 (ignore-patterns  ("*.wsdl" "*.class"))
                 (tags-file        ,(concat pd "TAGS"))
                 (open-files-cache ,(concat pd ".mk-project-open-files"))
                 (file-list-cache  ,(concat pd "file-list-cache") nil)
                 (ack-args         "--java")
                 (compile-cmd      write-soasm-build-script)
                 (startup-hook     soasm-startup)
                 (shutdown-hook    soasm-shutdown)
                 (vcs              git))))

;;; --------------------------------------------------------------------
;;; SOASM Utils
;;; --------------------------------------------------------------------

(defun w32-path (path)
  (let ((newpath (shell-command-to-string (concat "cygpath -w '" path "'"))))
    (substring newpath 0 (- (length newpath) 1)))) ; chomp

;; TODO: should this write a Makefile instead of a script?

(defun write-soasm-build-script ()
  "Write a .bat file to compile the current SOA view."
  (interactive)
  (mk-proj-assert-proj)
  (let* ((twelve-one-p (string-match "12.1" mk-proj-name))
         (twelve-five-p (string-match "12.5" mk-proj-name))
         (shortcut-p (y-or-n-p "Shortcut build? "))
         (clean-p (not (y-or-n-p "Skip Cleaning? ")))
         (build-type (if (y-or-n-p "Release build? ") "-release" "-debug"))
         (default-build-view-p (y-or-n-p "Default build view? "))
         (ps-build-view (when (not default-build-view-p)
                          (read-string "PS_BUILD_VIEW? "
                                       (if twelve-one-p
                                           soasm-121-ps-build-view
                                         soasm-125-ps-build-view))))
         (build-script (expand-file-name (concat "~/" mk-proj-name "-build.bat")))
         (view-root-dir (w32-path (replace-regexp-in-string "/devel/" "" mk-proj-basedir)))
         (devel-dir (w32-path mk-proj-basedir))
         (go-home (concat "cd " devel-dir "\n"))
         (static-view-p (string-match "cc-views" view-root-dir)))

    (flet ((clearmake-compile (dir &optional clean-target compile-target)
             (when (null clean-target) (setq clean-target "clean"))
             (when (null compile-target) (setq compile-target ""))
             (insert (concat "echo COMPILING " dir "\n"))
             (insert (concat "cd " devel-dir  dir "\n"))
             (when clean-p (insert "call clearmake -VF " clean-target "\n"))
             (insert "call clearmake -VF " compile-target "\n")
             (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n")))

    (with-temp-file build-script
      (insert "echo on\n")

      (when (not default-build-view-p)
        ;; TM_* unused
        ;; "call set TM_BUILD_VIEW=" ps-build-view "\n"
        ;; "call set TM_VIEW_PREFIX=%TM_BUILD_VIEW%\\devel\n"
        (insert "call set PS_BUILD_VIEW=" (if shortcut-p ps-build-view view-root-dir) "\n")
        (insert "call set PS_VIEW_PREFIX=%PS_BUILD_VIEW%\\devel\n")
        (insert "call set WA_BUILD_VIEW=" (if shortcut-p ps-build-view view-root-dir) "\n")
        (insert "call set WA_VIEW_PREFIX=%WA_BUILD_VIEW%\\devel\n"))

      (unless (string-equal (substring devel-dir 0 1) "C") ; goto the correct drive
        (insert (concat (substring devel-dir 0 1) ":\n")))
      (insert go-home)

      (insert "cd tools\\bin\n")
      (insert "call buildenv " build-type "\n\n")

      (unless shortcut-p
        (dolist (d '("common" "sdk" "xps" "policy-server" "agentcommon"
                     "agentframework" "affl-minder\\fedcommon"
                     "affl-minder\\policy-server"))
          (clearmake-compile d)))

      (clearmake-compile "transactionminder")

      (dolist (d '("app-server\\was\\scripts"))
        (insert (concat "echo COMPILING " d "\n"))
        (insert "REM Don't forget to edit AGENTCOMMON_VIEW and SDK_VIEW in build-all.cmd to point toward your local view.\n")
        (insert (concat "cd " devel-dir  d "\n"))
        (insert "REM Must always run clean before building was\n")
        (insert "call build-all.cmd clean\n")
        (insert "call build-all.cmd\n")
        (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n"))

      ;; (dolist (d '("app-server\\sspi\\scripts"))
      ;;   (insert (concat "echo COMPILING " d "\n"))
      ;;   (insert (concat "cd " devel-dir  d "\n"))
      ;;   (insert "REM Must always run clean before building sspi\n")
      ;;   (insert "call build-all.cmd clean\n")
      ;;   (insert "call build-all.cmd\n")
      ;;   (insert "REM Oh no! ERRORLEVEL isn't set correctly!\n")
      ;;   (insert "REM IF NOT ERRORLEVEL 0 GOTO ERROR\n\n"))

      ;; (clearmake-compile "asaframework")

      (let ((dirs (if twelve-five-p '("soaagent" "soamgr") '("soaagent"))))
        (dolist (d dirs)
          (clearmake-compile d)))

      (when twelve-one-p
        (clearmake-compile "soamgr\\soaadmin" "antclean" "")
        (insert "GOTO DONE\n\n"))

      (insert ":ERROR\necho Compile FAILED\n")
      (insert ":DONE\n"))

    (assert (file-readable-p build-script))

    ;; ;; run the build script in compile mode
    ;; (let* ((default-directory (file-name-directory build-script))
    ;;        (bat-script (file-name-nondirectory build-script)))
    ;; (compile (concat "cmd /c " bat-script) t))))
    )))

(defun clearmake-compile-writer ()
  (insert (concat "echo COMPILING " d "\n"))
  (insert (concat "cd " devel-dir  d "\n"))
  (when clean-p (insert "call clearmake -VF clean\n"))
  (insert "call clearmake -VF\n")
  (insert "IF NOT ERRORLEVEL 0 GOTO ERROR\n\n"))


(defun jar-for-current-file ()
  "Find this class in the project's many, many jar files"
  ;; TODO: searching from the top level is REALLY slow. Maybe try
  ;; searching from the /devel/X directory the file lives in first.
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when (string-match-p "\.java$" filename)
      (setq filename (replace-regexp-in-string "\.java$" ".class" filename)))
    (find-class-in-jars mk-proj-basedir filename)))


(defun sts-ant (&optional target)
  "Build STS with Ant"
  (interactive)
  (mk-proj-assert-proj)
  (let ((oldbuf (get-buffer "*ant-compilation*")))
    (if (not (null oldbuf))
        (kill-buffer "*ant-compilation*")))
  (let* ((default-directory (file-name-directory (concat mk-proj-basedir "soamgr/soaservices/sts/")))
         (buildfile (concat default-directory "build.xml"))
         (outbuf (get-buffer-create "*ant-compilation*"))
         (curbuf (current-buffer))
         (junit-jar (concat mk-proj-basedir "thirdparty/junit/3.8.1/junit.jar"))
         (target (if target
                     target
                   (ido-completing-read "Ant target: " '("clean" "compile" "dist" "test" "deploy-jboss5" "deploy-websphere7" "deploy-weblogic11")))))
    (switch-to-buffer-other-window outbuf)
    (insert "#> ant -emacs -lib " junit-jar " -f " buildfile " " target "\n")
    (switch-to-buffer-other-window curbuf)
    (call-process "ant" nil outbuf t "-emacs" "-lib" junit-jar "-f" buildfile target)))

(defun fix-mbk()
  (interactive)

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.FINEST, " "logger.debug(") ; we don't have .trace()?

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.DEBUG, " "logger.debug(")

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.WARNING, " "logger.warn(")

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.ERROR, " "logger.error(")

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.SEVERE, " "logger.error(") ; FATAL?

  (goto-char (point-min))
  (query-replace-regexp "logger\\.log(MbkLogLevel.INFO, " "logger.info(")

  (goto-char (point-min))
  (query-replace-regexp "MbkLogger\\.logDebug" "logger.isDebugEnabled"))

(let ((pd (project-meta-dir "pmws")))
(project-def "pmws"
   `((basedir ,(concat homedir "Desktop/pmws-proto"))
     (src-patterns ("*.java"))
     (file-list-cache ,(concat pd "files"))
     (open-files-cache ,(concat pd "open-files"))
     (ignore-patterns ("*.class"))
     (tags-file ,(concat pd "TAGS"))
     (vcs git))))


(defun pmws-cc-project-def (name dir)
  (let* ((bd (file-name-as-directory dir))
         (pd (project-meta-dir name))
         (find-cmd (concat "find "
                           (concat bd "pmws ")
                           (concat bd "sdk ")
                           (concat bd "webadmin ")
                           "-name '*.java' -type f")))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (not (file-readable-p bd))
        (message "Warning: view %s does not exist" bd))
    (message "Defining pmws project %s at basedir %s" name bd)
    (project-def name
                 `((basedir          ,bd)
                   (src-patterns     ("*.java$"))
                   (ignore-patterns  ("*.wsdl" "*.class" "*.obj" "*.o" ".so"))
                   (src-find-cmd     ,find-cmd)
                   (grep-find-cmd    ,find-cmd)
                   (index-find-cmd   ,find-cmd)
                   (tags-file        ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache  ,(concat pd "file-list-cache"))
                   (ack-args         "--java")
                   (compile-cmd      "ant")))))

(defun pmws-git-project-def (name dir clt-dir)
  (let* ((bd (file-name-as-directory dir))
         (pd (project-meta-dir name))
         (find-cmd (concat "find " bd " " (file-name-as-directory clt-dir) "devel/sdk -name '*.java' -type f")))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (not (file-readable-p bd))
        (message "Warning: view %s does not exist" bd))
    (message "Defining pmws project %s at basedir %s" name bd)
    (project-def name
                 `((basedir          ,bd)
                   (src-patterns     ("*.java$"))
                   (ignore-patterns  ("*.wsdl" "*.class" "*.obj" "*.o" ".so"))
                   (src-find-cmd     ,find-cmd)
                   (grep-find-cmd    ,find-cmd)
                   (index-find-cmd   ,find-cmd)
                   (tags-file        ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache  ,(concat pd "file-list-cache"))
                   (ack-args         "--java")
                   (vcs              git)
                   (compile-cmd      "ant")))))

(pmws-cc-project-def "pmws-validation" "m:/kelma12-r12.5-pmws-validation/devel")
(pmws-cc-project-def "pmws-authschemes" "m:/kelma12-r12.5-pmws-authschemes/devel")
;(pmws-git-project-def "pmws-authschemes" "c:/git/pmws-authschemes" "m:/kelma12-r12.5-pmws-authschemes")

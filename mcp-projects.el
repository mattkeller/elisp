;;;; mcp-projects: machinery to define MCP mk-projects autoamtically
;;;;   and setup their JDE environments

(require 'mk-project)

;;; --------------------------------------------------------------------
;;; Utils
;;; --------------------------------------------------------------------

(defun homedir () (concat (getenv "HOME") "/"))

(defun mktop-p ()
  (string-equal system-name "mktop"))

(defun normalize-dirname (dir)
  "Ensure dir ends in '/'"
  (let* ((len (length dir))
         (last-char (substring dir (- len 1) len)))
    (if (string-equal last-char "/")
        dir
      (concat dir "/"))))

;;; --------------------------------------------------------------------
;;; MCP JDE utilities
;;; --------------------------------------------------------------------

(defvar mcp10-jars (mapcar '(lambda (j) (concat "mcp_3rdParty/java/" j))
                           '("database/oracle/oracle.zip"
                             "management/jdmk/jawall.jar"
                             "megaco/megaco.jar"
                             "parsing/jdom/jdom.jar"
                             "uas/uasemClient.jar"
                             "tools/sun/tools.jar"
                             "axis/axis.jar"
                             "axis/OPIClient.jar"
                             "tomcat/tomcat.jar"
                             "httpclient/commons-httpclient-2.0-rc2.jar"
                             "management/jdmk/jsnmpapi.jar"
                             "management/jdmk/jdmkrt.jar"
                             "security/bcprov-jdk14-124.jar"
                             "masSoapServices/masservice.jar"
                             "jazzlib/jazzlib.jar")))



(defun mcp-jde-jars-from-build-xml (basedir)
  "Return the list of 'standard.jars' from build.xml"
  (let* ((jars nil)
         (root (with-temp-buffer
                 (insert-file-contents (concat basedir "mcp_core_root/loadbuild/scripts/build.xml"))
                 (xml-parse-region (point-min) (point-max))))
         (project (car root)))
    (dolist (c (xml-get-children project 'path))
      (let ((attrs (xml-node-attributes c)))
        (when (and attrs (assq 'id attrs))
          (when (string-equal "standard.jars" (cdr (assq 'id attrs)))
            (dolist (elem (xml-get-children c 'pathelement))
              (let ((jar (cdr (assq 'location (xml-node-attributes elem)))))
                (when (string-match "${3pjars.dir}." jar)
                  (setq jar (replace-match (concat basedir "mcp_3rdParty/lib/jars/") nil nil jar)))
                (push jar jars)))))))
    jars))

(defvar mcp-ant-home (if (mktop-p) "/opt/ant-1.7.0" "/localdisk/data/matthewk/local/ant-1.7.0"))

(defvar mcp-ant-bin  (concat mcp-ant-home "/bin/ant"))

;; TODO: fish these out of build.xml
(defvar mcp-ant-jars (mapcar (lambda (j) (concat mk-proj-basedir j))
                                         '("mcp_3rdParty/lib/jars/build/mcpant.jar"
                                           "mcp_3rdParty/lib/jars/build/antlr.jar"
                                           "mcp_3rdParty/lib/jars/optional/axis.jar")))

(defun mcp-jde-setup (basedir workdir stagedir &optional jar-list jdk)
  "Setup JDE for a particular project. Defaults to mcp12 jar-list and preferred jdk."
  (require 'jde)
  (let ((classpath (if (null jar-list) (mcp-jde-jars-from-build-xml basedir) jar-list))
        (classdir (concat workdir "classes")))
    (push (concat workdir "client-compiled/sopi") classpath)
    (push (concat workdir "client-compiled/omi")  classpath)
    (push (concat workdir "client-compiled/opi")  classpath)
    (setq
     ;; For jde-compile
     jde-global-classpath         (cons classdir classpath)
     jde-compiler                 '("javac server" "")
     jde-compile-option-directory  classdir
     jde-compile-option-sourcepath (list (concat mk-proj-basedir "mcp_core_root/src"))
     ;; for jde-ant-build
     jde-ant-home                  mcp-ant-home
     jde-ant-program               mcp-ant-bin
     jde-ant-user-jar-files        mcp-ant-jars
     jde-ant-args                  (concat "-emacs -Dbuild.compiler.emacs=true -Dno.web.client=true "
                                          (format "-Dcompile.destination=%s " classdir)
                                          (format "-Dstaging.dir=%s " stagedir)
                                          (format "-Dworking.dir=%s " workdir)
                                          "-Dant.build.javac.target=1.6 -Dant.build.javac.source=1.6 ")
    jde-ant-buildfile             (concat mk-proj-basedir "mcp_core_root/loadbuild/scripts/build.xml")
    jde-ant-working-directory     (concat mk-proj-basedir "mcp_core_root/loadbuild/scripts/")
    jde-ant-read-target           t
    jde-ant-complete-target       t)))

;;; --------------------------------------------------------------------
;;; MCP Project Auto loader utils
;;; --------------------------------------------------------------------

;;; These functions enable me to automatically create mk-project
;;; projects based on my mcpant (~/.*.ant) project files. This removes
;;; a ton of duplicated project-def statements as well as making it
;;; very easy to create new projects. Only startup and shutdown hooks
;;; need to stay in this file.

(defvar mcp-proj-dirs (make-hash-table :test 'equal)
  "Map proj-name to (viewdir workdir stagedir) lists")

(defun mcp-proj-dir-add (name viewdir workdir stagedir)
  "Add a project's viewdir/workdir info to our hash"
  (puthash name (list viewdir workdir stagedir) mcp-proj-dirs))

(defun mcp-proj-dir-get (name)
  "Retreive (viewdir workdir) for project `name'"
  (gethash name mcp-proj-dirs))

(defun mcp-proj-generic-startup-hook ()
  "Run the user-defined startup hook if it exists, or generically
set up JDE for the current project"
  (let* ((hook-name (concat mk-proj-name "-startup-hook"))
         (hook-sym (intern hook-name)))
    (if (functionp hook-sym)
        (funcall hook-sym)
      (let ((proj-dirs (mcp-proj-dir-get mk-proj-name)))
        (when proj-dirs
          (message "Running generic mcp-jde-setup for %s" mk-proj-name)
          (mcp-jde-setup (first proj-dirs) (second proj-dirs) (third proj-dirs)))))))

(defun mcp-proj-generic-shutdown-hook ()
  "Run the user-defined shutdown hook if it exists"
  (let* ((hook-name (concat mk-proj-name "-shutdown-hook"))
         (hook-sym (intern hook-name)))
    (when (functionp hook-sym)
      (funcall hook-sym))))

(defun mcp-proj-auto-project-def (name)
  "Define a mk-project based on ~/.`name'.ant. Predefined Startup
  & shutdown hooks can be <name>-startup-hook and
  <name>-startup-hook."
  (interactive "sProject Name: ")
  (let* ((proj-file (concat (homedir) "." name ".ant"))
         (lst (mcp-proj-parse-project-file proj-file)))
    (let ((viewdir (first lst))
          (workdir (second lst))
          (stagedir (third lst)))
      (if (and workdir viewdir stagedir)
          (let* ((startup-hook-name (concat name "-startup-hook"))
                 (shutdown-hook-name (concat name "-shutdown-hook")))
            (mcp-proj-dir-add name viewdir workdir stagedir)
            (project-def name
                         `((basedir ,viewdir)
                           (src-patterns ("*.java" "*.jsp"))
                           (ignore-patterns ("*.class" "*.wsdl"))
                           (tags-file ,(concat (homedir) ".TAGS-" name))
                           (file-list-cache ,(concat (homedir) "." name "-files"))
                           (open-files-cache ,(concat (homedir) "." name "-open-files"))
                           (compile-cmd ,(concat "mcpant " name))
                           (startup-hook mcp-proj-generic-startup-hook)
                           (shutdown-hook mcp-proj-generic-shutdown-hook)
                           (vcs git))) ;; TODO: assuming all mcp projects use git for now
            (message "Created MCP project %s. View is %s, work is %s, stage is %s" name viewdir workdir stagedir))
        (message "Sorry, can't parse %s MCP project file" proj-file)))))

(defun mcp-proj-parse-project-file (proj-file)
  "Given a mcp ant project file, return '(viewdir workdir stagedir)"
    (let (viewdir workdir stagedir)
      (unless (file-readable-p proj-file)
        (error (concat "Unable to read " proj-file ".")))
      (with-temp-buffer
        (insert-file-contents proj-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((start (point)))
            (while (not (eolp)) (forward-char)) ; goto end of line
            (let ((line (buffer-substring start (point))))
              (when (string-match "[ \t]*$" line) ; trim whitespace
                (setq line (replace-match "" nil nil line)))
              (let ((lst (split-string line "="))) ; split line
                (when lst
                  (when (string-equal (car lst) "VIEW_DIR")
                    (setq viewdir (car (cdr lst))))
                  (when (string-equal (car lst) "STAGE_DIR")
                    (setq stagedir (car (cdr lst))))
                  (when (string-equal (car lst) "WORK_DIR")
                    (setq workdir (car (cdr lst))))))))
          (forward-line)))
      (list (normalize-dirname viewdir) (normalize-dirname workdir) stagedir)))

(defun mcp-proj-load-all-projects ()
  "Load all projects we have ~/.*.ant files for"
  (interactive)
  (mapcar (lambda (f)
            (when (string-match "^\\." f)
              (setq f (replace-match "" nil nil f))
              (when (string-match "\\.ant" f)
                (setq f (replace-match "" nil nil f))
                  (mcp-proj-auto-project-def f))))
          (directory-files (getenv "HOME") nil ".*.ant" t)))

;; Load them all!
(mcp-proj-load-all-projects)

(provide 'mcp-projects)

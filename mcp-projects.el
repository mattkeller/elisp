;;;; mcp-projects: machinery to define MCP mk-projects autoamtically
;;;;   and setup their JDE environments

(require 'mk-project)

;;; --------------------------------------------------------------------
;;; Utils
;;; --------------------------------------------------------------------

(defun homedir () (concat (getenv "HOME") "/"))

(defun mktop-p ()
  (string-equal system-name "mktop"))

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

(defvar mcp-jde-preferred-jdk (if (mktop-p) "1.6.0_13" "1.6.0_11")
  "The preferred mcp12 jdk to use per machine")

(defvar mcp-jde-jdk-registry (if (mktop-p)
                        '(("1.6.0_13" . "/opt/jdk1.6.0_13"))
                      '(("1.5.0_11" . "/localdisk/jdk1.5.0_11")
                        ("1.6.0_05" . "/localdisk/jdk1.6.0_05")
                        ("1.6.0_11" . "/localdisk/data/matthewk/local/jdk1.6.0_11"))))

(defun mcp-jde-jars-from-build-xml (basedir)
  "Return the list of 'standard.jars' from build.xml"
  (let* ((jars nil)
         (root (with-temp-buffer
                 (insert-file-contents (concat basedir "/mcp_core_root/loadbuild/scripts/build.xml"))
                 (xml-parse-region (point-min) (point-max))))
         (project (car root)))
    (dolist (c (xml-get-children project 'path))
      (let ((attrs (xml-node-attributes c)))
        (when (and attrs (assq 'id attrs))
          (when (string-equal "standard.jars" (cdr (assq 'id attrs)))
            (dolist (elem (xml-get-children c 'pathelement))
              (let ((jar (cdr (assq 'location (xml-node-attributes elem)))))
                (when (string-match "${3pjars.dir}." jar)
                  (setq jar (replace-match (concat basedir "/mcp_3rdParty/lib/jars/") nil nil jar)))
                (push jar jars)))))))
    jars))

(defun mcp-jde-setup (basedir classdir &optional jar-list jdk)
  "Setup JDE for a particular project. Defaults to mcp12 jar-list and preferred jdk."
  (require 'jde)
  (let ((classpath (mapcar '(lambda (j)
                              (concat mk-proj-basedir j))
                           (if (null jar-list) (mcp-jde-jars-from-build-xml basedir) jar-list))))
    (setq jde-jdk                       (list (if (null jdk) mcp-jde-preferred-jdk jdk)))
          jde-jdk-registry              mcp-jde-jdk-registry
          jde-global-classpath          (push  classdir classpath)                     ;; Classpaths for compile, run, and debug cmds.
          jde-sourcepath                (list (concat basedir "/mcp_core_root/src"))   ;; Path of source files for compile, run and debug.
          jde-compile-option-classpath   nil                                           ;; Classpath for compile cmd. If set, overrides jde-global-classpath.
          jde-compile-option-directory  classdir                                       ;; Directory into which to place the compiled class.
          jde-compile-option-sourcepath (list (concat basedir "/mcp_core_root/src")))) ;; Path of sources required to compile the current class.


;;; --------------------------------------------------------------------
;;; MCP Project Auto loader utils
;;; --------------------------------------------------------------------

;;; These functions enable me to automatically create mk-project
;;; projects based on my mcpant (~/.*.ant) project files. This removes
;;; a ton of duplicated project-def statements as well as making it
;;; very easy to create new projects. Only startup and shutdown hooks
;;; need to stay in this file.

(defvar mcp-proj-dirs (make-hash-table :test 'equal)
  "Map proj-name to (viewdir workdir) pairs")

(defun mcp-proj-dir-add (name viewdir workdir)
  "Add a project's viewdir/workdir info to our hash"
  (puthash name (list viewdir workdir) mcp-proj-dirs))

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
          (mcp-jde-setup (first proj-dirs) (second proj-dirs)))))))

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
          (workdir (second lst)))
      (if (and workdir viewdir)
          (let* ((startup-hook-name (concat name "-startup-hook"))
                 (shutdown-hook-name (concat name "-shutdown-hook")))
            (mcp-proj-dir-add name viewdir workdir)
            (project-def name
                         `((basedir ,viewdir)
                           (src-patterns ("*.java" "*.jsp"))
                           (ignore-patterns ("*.class" "*.wsdl"))
                           (tags-file ,(concat (homedir) ".TAGS-" name))
                           (file-list-cache ,(concat (homedir) "." name "-files"))
                           (compile-cmd ,(concat "mcpant " name))
                           (startup-hook mcp-proj-generic-startup-hook)
                           (shutdown-hook mcp-proj-generic-shutdown-hook)
                           (vcs git))) ;; TODO: assuming all mcp projects use git for now
            (message "Created MCP project %s. View is %s, work is %s." name viewdir workdir))
        (message "Sorry, can't parse %s MCP project file" proj-file)))))

(defun mcp-proj-parse-project-file (proj-file)
  "Given a mcp ant project file, return '(viewdir workdir)"
    (let ((viewdir nil)
          (workdir nil))
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
                  (when (string-equal (car lst) "WORK_DIR")
                    (setq workdir (car (cdr lst))))))))
          (forward-line)))
      (list viewdir workdir)))

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

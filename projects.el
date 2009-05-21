;;;; projects.el --- my mk-project settings

(require 'mk-project)
(require 'cl)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(defvar homedir (concat (getenv "HOME") "/"))

;;; --------------------------------------------------------------------
;;; cl-sip
;;; --------------------------------------------------------------------

(defvar sip-basedir (concat homedir "code/lisp/cl-sip/"))

(project-def "cl-sip"
             `((basedir ,sip-basedir)
               (src-patterns (".lisp" "*.asd"))
               (ignore-patterns ("*.fasl"))
               (tags-file ,(concat sip-basedir "TAGS"))
               (vcs git)
               (startup-hook cl-sip-startup-hook)))

(defun cl-sip-startup-hook ()
  (slime)
  (dolist (file (directory-files sip-basedir t "\.lisp$" t))
    (find-file file)))

;;; --------------------------------------------------------------------
;;; elisp
;;; --------------------------------------------------------------------

(project-def "elisp"
             `((basedir ,(concat homedir "elisp/"))
               (src-patterns ("*.el"))
               (file-list-cache ,(concat homedir ".elisp-files"))
               (ignore-patterns ("*.elc"))
               (tags-file ,(concat homedir "elisp/" "TAGS"))
               (vcs git)
               (startup-hook mk-project-startup-hook)))

(defun mk-project-startup-hook ()
  (find-file (concat mk-proj-basedir "dotemacs"))
  (find-file (concat mk-proj-basedir "mk-project.el")))

;;; --------------------------------------------------------------------
;;; Qrev
;;; --------------------------------------------------------------------

(project-def "qrev"
             '((basedir "~mk/code/lisp/qrev/")
               (src-patterns ("*.lisp"))
               (ignore-patterns ("*.fasl"))
               (tags-file "~mk/code/lisp/qrev/TAGS")
               (vcs git)
               (compile-cmd "make -k")
               (startup-hook qrev-startup-hook)
               (shutdown-hook qrev-shutdown-hook)))

(defun qrev-startup-hook ()
  (slime)
  (dolist (file (directory-files mk-proj-basedir t "\.lisp$" t))
    (find-file file))
  (message "Qrev it up, baby!"))

(defun qrev-shutdown-hook ()
  (when (y-or-n-p "Close slime? ")
    (slime-quit-lisp)))

;;; --------------------------------------------------------------------
;;; Scplite (MCS)
;;; --------------------------------------------------------------------

(project-def "scplite.mcs"
             '((basedir "/home/matthewk/git/scplite.mcs.git")
               (src-patterns ("*.C" "*.H" "*.c" "*.h"))
               (ignore-patterns ("*.o"))
               (tags-file "/home/matthewk/git/scplite.mcs.git/TAGS")
               (file-list-cache "/home/matthewk/.scplite.mcs.files")
               (compile-cmd "./buildphx.sh")
               (vcs git)))

;;; --------------------------------------------------------------------
;;; MCP project utils
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

(defun mktop-p ()
  (string-equal system-name "mktop"))

(defvar mcp12-preferred-jdk (if (mktop-p) "1.6.0_13" "1.6.0_11")
  "The preferred mcp12 jdk to use per machine")

(defvar mcp-jdk-reg (if (mktop-p)
                        '(("1.6.0_13" . "/opt/jdk1.6.0_13"))
                      '(("1.5.0_11" . "/localdisk/jdk1.5.0_11")
                        ("1.6.0_05" . "/localdisk/jdk1.6.0_05")
                        ("1.6.0_11" . "/localdisk/data/matthewk/local/jdk1.6.0_11"))))

(defun mcp-get-jars-from-build-xml (basedir)
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
                           (if (null jar-list) (mcp-get-jars-from-build-xml basedir) jar-list))))
    (setq jde-compile-option-directory classdir)
    (setq jde-compile-option-sourcepath (list (concat mk-proj-basedir "/mcp_core_root/src")))
    (setq jde-global-classpath (push  classdir classpath))
    (setq jde-jdk-registry mcp-jdk-reg)
    (setq jde-jdk (list (if (null jdk) mcp12-preferred-jdk jdk)))))

;;; --------------------------------------------------------------------
;;; mcp12 projects
;;; --------------------------------------------------------------------

(defun mcp-12dyn-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_mcp_core_12.0_3/work/classes")
  (find-file "~/proj/geol3/NOTES")
  (find-file "/mcp/mcp_core_ims/ims/foundation/url/CommonURL.java")
  (find-file "/mcp/mcp_core_ims/ims/cap/svc/iptel/eventhandler/IPTelHandlerNullAuthOrig.java"))

;;; --------------------------------------------------------------------
;;; AGCF projects
;;; --------------------------------------------------------------------

(defun agcf-git-startup-hook ()
  (message "Hi from agcf-git-startup-hook")
  (mcp-jde-setup "/localdisk/data/matthewk/git/agcf-static.git/mcp"
                 "/localdisk/data/matthewk/ant/agcf-git/work/classes")
  (find-file "/localdisk/data/matthewk/git/agcf-static.git/mcp/mcp_labs/common/test_tools/agcf/configureSuite.pl"))

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

(defun mcp-generic-startup-hook ()
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

(defun mcp-generic-shutdown-hook ()
  "Run the user-defined shutdown hook if it exists"
  (let* ((hook-name (concat mk-proj-name "-shutdown-hook"))
         (hook-sym (intern hook-name)))
    (when (functionp hook-sym)
      (funcall hook-sym))))

(defun mcp-auto-project-def (name)
  "Define a mk-project based on ~/.`name'.ant. Predefined Startup
  & shutdown hooks can be <name>-startup-hook and
  <name>-startup-hook."
  (interactive "sProject Name: ")
  (let* ((proj-file (concat homedir "." name ".ant"))
         (lst (mcp-parse-project-file proj-file)))
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
                           (tags-file ,(concat homedir ".TAGS" name))
                           (file-list-cache ,(concat homedir "." name "-files"))
                           (compile-cmd ,(concat "mcpant " name))
                           (startup-hook mcp-generic-startup-hook)
                           (shutdown-hook mcp-generic-shutdown-hook)
                           (vcs git))) ;; TODO: assuming all mcp projects use git for now
            (message "Created MCP project %s. View is %s, work is %s." name viewdir workdir))
        (message "Sorry, can't parse %s MCP project file" proj-file)))))

(defun mcp-parse-project-file (proj-file)
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

(defun mcp-load-all-projects ()
  "Load all projects we have ~/.*.ant files for"
  (interactive)
  (mapcar (lambda (f)
            (when (string-match "^\\." f)
              (setq f (replace-match "" nil nil f))
              (when (string-match "\\.ant" f)
                (setq f (replace-match "" nil nil f))
                  (mcp-auto-project-def f))))
          (directory-files (getenv "HOME") nil ".*.ant" t)))

;; Load them all!
(mcp-load-all-projects)

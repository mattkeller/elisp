;;;; projects.el --- my mk-project settings

(require 'mk-project)

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

(defvar mcp12-jars (mapcar '(lambda (j) (concat "mcp_3rdParty/" j))
                           '("core/3rdParty.jar"
                             "optional/tools.jar"
                             "build/log4j.jar"
                             "optional/axis.jar"
                             "optional/tomcat.jar"
                             "optional/masservice.jar"
                             "optional/stinger.jar"
                             "core/muse.jar"
                             "core/commons-logging.jar"
                             "build/webserv-ext.jar"
                             "build/webserv-rt.jar"
                             "optional/commons-beanutils-1.8.0-BETA.jar"
                             "optional/jsf-api.jar"
                             "optional/portal-portlet-jsr168api-lib.jar"
                             "optional/jboss-jmx.jar"
                             "optional/jbossall-client.jar"
                             "optional/jboss-jaxrpc.jar"
                             "optional/portal-api-lib.jar"
                             "optional/jboss-system.jar"
                             "optional/portal-common-portal-lib.jar"
                             "optional/portal-theme-lib.jar"
                             "build/commons-codec.jar"
                             "build/jboss-aop-jdk50.jar"
                             "optional/jboss-jaxws.jar"
                             "optional/jaxb-api.jar"
                             "core/3rdParty.jar"
                             "build/httpservlet-event.jar"
                             "build/httpservlet-ra.jar"
                             "build/httpservlet-ratype.jar"
                             "build/mobicents_lib.jar"
                             "build/sip-ra.jar"
                             "build/sip-ratype.jar"
                             "build/sip-event.jar"
                             "build/jain-sip-api-1.2.1.jar"
                             "build/jain-sip-ri-1.2.1.jar"
                             "build/slee_1_1.jar"
                             "optional/OPIStubs.jar"
                             "optional/jsf-impl.jar"
                             "optional/jbosssx.jar"
                             "optional/jboss-deployment.jar"
                             "build/jgroups.jar"
                             "build/jboss-cache-jdk50.jar"
                             "build/servlet-api.jar"
                             "build/trove.jar"
                             "optional/richfaces-api-3.1.4.GA.jar"
                             "optional/richfaces-ui-3.1.4.GA.jar"
                             "optional/richfaces-impl-3.1.4.GA.jar"
                             "optional/portletbridge-api-1.0.0.B1.jar"
                             "optional/portletbridge-impl-1.0.0.B1.jar"
                             "optional/portal-web-lib.jar"
                             "optional/commons-discovery-0.2.jar"
                             "optional/axis-jboss.jar"
                             "build/concurrent.jar"
                             "build/javassist.jar"
                             "build/jboss-aop-jdk50-client.jar"
                             "build/jboss-aspect-jdk50-client.jar"
                             "build/jboss-aspect-library-jdk50.jar"
                             "build/jboss-common.jar"
                             "build/jrockit-pluggable-instrumentor.jar"
                             "build/pluggable-instrumentor.jar"
                             "build/qdox.jar"
                             "optional/hibernate3.jar"
                             "optional/jboss-hibernate.jar"
                             "lib/jars/optional/junit-4.5.jar")))

(defun mktop-p ()
  (string-equal system-name "mktop"))

(defvar mcp12-preferred-jdk (if (mktop-p) "1.6.0_13" "1.6.0_11")
  "The preferred mcp12 jdk to use per machine")

(defvar mcp-jdk-reg (if (mktop-p)
                        '(("1.6.0_13" . "/opt/jdk1.6.0_13"))
                      '(("1.5.0_11" . "/localdisk/jdk1.5.0_11")
                        ("1.6.0_05" . "/localdisk/jdk1.6.0_05")
                        ("1.6.0_11" . "/localdisk/data/matthewk/local/jdk1.6.0_11"))))

(defun mcp-jde-setup (basedir classdir &optional jar-list jdk)
  "Setup JDE for a particular project. Defaults to mcp12 jar-list and jdk."
  (require 'jde)
  (let ((classpath (mapcar '(lambda (j)
                              (concat mk-proj-basedir j))
                           (if (null jar-list) mcp12-jars jar-list))))
    (setq jde-compile-option-directory classdir)
    (setq jde-compile-option-sourcepath (list (concat mk-proj-basedir "mcp_core_root/src")))
    (setq jde-global-classpath (push  classdir classpath))
    (setq jde-jdk-registry mcp-jdk-reg)
    (setq jde-jdk (list (if (null jdk) mcp12-preferred-jdk jdk)))))

;;; --------------------------------------------------------------------
;;; mcp12 projects
;;; --------------------------------------------------------------------

(project-def "12dyn"
             '((basedir "/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGS12")
               (file-list-cache "/home/matthewk/.12dyn-files")
               (compile-cmd "mcpant 12dyn")
               (startup-hook mcp-12dyn-startup-hook)
               (shutdown-hook nil)))

(defun mcp-12dyn-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_mcp_core_12.0_3/work/classes")
  (find-file "~/proj/geol3/NOTES")
  (find-file "/mcp/mcp_core_ims/ims/foundation/url/CommonURL.java")
  (find-file "/mcp/mcp_core_ims/ims/cap/svc/iptel/eventhandler/IPTelHandlerNullAuthOrig.java"))

;;; --------------------------------------------------------------------
;;; AGCF projects
;;; --------------------------------------------------------------------

(project-def "agcf12sp1"
             '((basedir "/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGSagcf12sp1")
               (file-list-cache "/home/matthewk/.agcf12sp1-files")
               (compile-cmd "mcpant agcf12sp1")
               (startup-hook agcf12sp1-startup-hook)
               (shutdown-hook nil)))

(defun agcf12sp1-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_AGCF_mcp_core_12.0_sp1_dev/work/classes"))

(project-def "agcf-static"
             '((basedir "/localdisk/viewstore/matthewk_AGCF_mcp12sp1_static/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGSagcf-static")
               (file-list-cache "/home/matthewk/.agcf-static-files")
               (compile-cmd "mcpant agcf-static")
               (startup-hook agcf-static-startup-hook)
               (shutdown-hook nil)
               (vcs git)))

(defun agcf-static-startup-hook ()
  (mcp-jde-setup "/localdisk/viewstore/matthewk_AGCF_mcp12sp1_dev_static/mcp/"
                 "/localdisk/data/matthewk/ant/agcf-static/work/classes"))

(project-def "agcf-static-home"
             '((basedir "/home/mk/nt/agcf-static/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/mk/.TAGSagcf-static")
               (file-list-cache "/home/mk/.agcf-static-files")
               (compile-cmd "mcpant agcf-static")
               (startup-hook agcf-static-home-startup-hook)
               (shutdown-hook nil)
               (vcs git)))

(defun agcf-static-home-startup-hook ()
  (mcp-jde-setup "/home/mk/nt/agcf-static/mcp/"
                 "/home/mk/nt/ant/agcf-static/work/classes"))

(project-def "agcf-mct-home"
             '((basedir "/home/mk/nt/agcf-mct/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/mk/.TAGSagcf-mct")
               (file-list-cache "/home/mk/.agcf-mct-files")
               (compile-cmd "mcpant agcf-mct")
               (startup-hook agcf-mct-home-startup-hook)
               (shutdown-hook nil)
               (vcs git)))

(defun agcf-mct-home-startup-hook ()
  (mcp-jde-setup "/home/mk/nt/agcf-mct/mcp/"
                 "/home/mk/nt/ant/agcf-mct/work/classes"))

(project-def "agcf-int"
             '((basedir "/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGS-agcf-int")
               (file-list-cache "/home/matthewk/.agcf-int.files")
               (compile-cmd "mcpant agcf-int")
               (startup-hook agcf-int-startup-hook)
               (shutdown-hook nil)))

(defun agcf-int-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_AGCF_mcp_12.0_int/work/classes"))

(project-def "agcf-git"
             '((basedir "/localdisk/data/matthewk/git/agcf-static.git/mcp")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGS-agcf-git")
               (file-list-cache "/home/matthewk/.agcf-git.files")
               (compile-cmd "mcpant agcf-git")
               (startup-hook agcf-git-startup-hook)
               (shutdown-hook nil)))

(defun agcf-git-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/agcf-git/work/classes")
  (find-file "/localdisk/data/matthewk/git/agcf-static.git/mcp/mcp_labs/common/test_tools/agcf/configureSuite.pl"))



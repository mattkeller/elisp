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

(project-def "elisp"
             `((basedir ,(concat (getenv "HOME") "/elisp/"))
               (src-patterns ("*.el"))
               (ignore-patterns ("*.elc"))
               (tags-file ,(concat (getenv "HOME") "/elisp/" "TAGS"))
               (git-p t)
               (startup-hook mk-project-startup-hook)))

(defun mk-project-startup-hook ()
  (find-file (concat mk-proj-basedir "dotemacs"))
  (find-file (concat mk-proj-basedir "mk-project.el")))

(project-def "qrev"
             '((basedir "~mk/code/lisp/qrev/")
               (src-patterns ("*.lisp"))
               (ignore-patterns ("*.fasl"))
               (tags-file "~mk/code/lisp/qrev/TAGS")
               (git-p t)
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
                             "optional/jboss-hibernate.jar")))

(defvar mcp-jdk-reg '(("1.5.0_11" . "/localdisk/jdk1.5.0_11")
                      ("1.6.0_05" . "/localdisk/jdk1.6.0_05")
                      ("1.6.0_07" . "/localdisk/data/matthewk/jdk1.6.0_07")))

(defun mcp-jde-setup (basedir classdir jar-list jdk)
  (require 'jde)
  (let ((classpath (mapcar '(lambda (j) (concat mk-proj-basedir j)) jar-list)))
    (setq jde-compile-option-directory classdir)
    (setq jde-compile-option-sourcepath (list (concat mk-proj-basedir "mcp_core_root/src")))
    (setq jde-global-classpath (push  classdir classpath))
    (setq jde-jdk-registry mcp-jdk-reg)
    (setq jde-jdk (list jdk))))


(project-def "12dyn"
             '((basedir "/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/home/matthewk/.TAGS12")
               (git-p nil)
               (compile-cmd "mcpant 12dyn")
               (startup-hook mcp-12dyn-startup-hook)
               (shutdown-hook nil)))

(defun mcp-12dyn-startup-hook ()
  (mcp-jde-setup "/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_mcp_core_12.0_3/classes"
                 mcp12-jars
                 "1.6.0_07")
  (find-file "~/proj/geol3/NOTES")
  (find-file "/mcp/mcp_core_ims/ims/foundation/url/CommonURL.java")
  (find-file "/mcp/mcp_core_ims/ims/cap/svc/iptel/eventhandler/IPTelHandlerNullAuthOrig.java"))


;;; OBSOLETE?

(defun mcp-set-proj (proj)
  (interactive "sProject name: ")
  (let* ((mcp-dir "/mcp/")
         (git-dir "/localdisk/data/matthewk/code/mcp.git/mcp/")
         (src-dir mcp-dir)
         (wrk-dir (concat "/localdisk/data/matthewk/workdir/" proj))
         (cls-dir (concat wrk-dir "/classes")))
    (when (y-or-n-p "Git project?")
      (setq src-dir git-dir))
    (if (file-directory-p src-dir)
        (progn
          (setq jde-compile-option-directory cls-dir)
          (setq jde-compile-option-sourcepath (list (concat src-dir "mcp_core_root/src")
                                                    (concat src-dir "mcp_core_root/src")
                                                    (concat src-dir "mcp_core_ims/ims")))
          (let* ((jars '("mcp_3rdParty/java/database/oracle/oracle.zip"
                         "mcp_3rdParty/java/management/jdmk/jawall.jar"
                         "mcp_3rdParty/java/megaco/megaco.jar"
                         "mcp_3rdParty/java/parsing/jdom/jdom.jar"
                         "mcp_3rdParty/java/uas/uasemClient.jar"
                         "mcp_3rdParty/java/tools/sun/tools.jar"
                         "mcp_3rdParty/java/axis/axis.jar"
                         "mcp_3rdParty/java/axis/OPIClient.jar"
                         "mcp_3rdParty/java/tomcat/tomcat.jar"
                         "mcp_3rdParty/java/httpclient/commons-httpclient-2.0-rc2.jar"
                         "mcp_3rdParty/java/management/jdmk/jsnmpapi.jar"
                         "mcp_3rdParty/java/management/jdmk/jdmkrt.jar"
                         "mcp_3rdParty/java/security/bcprov-jdk14-124.jar"
                         "mcp_3rdParty/java/masSoapServices/masservice.jar"
                         "mcp_3rdParty/java/jazzlib/jazzlib.jar"))
                 (classpath (mapcar '(lambda (j) (concat src-dir j)) jars)))
            (setq jde-global-classpath (push cls-dir classpath)))
          (setq jde-jdk-registry '(("1.5.0_07" . "/localdisk/jdk1.5.0_07")))
          (message "Change JDE paths for proj %s" proj)
          t)
      (progn
        (message "Can't read dir %s" src-dir)
        nil))))

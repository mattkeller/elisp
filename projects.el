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

(defvar mcp10-jars '("mcp_3rdParty/java/database/oracle/oracle.zip"
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

(defvar mcp12-jars '("mcp_3rdparty/core/3rdParty.jar"
                     "mcp_3rdparty/optional/tools.jar"
                     "mcp_3rdparty/build/log4j.jar"
                     "mcp_3rdparty/optional/axis.jar"
                     "mcp_3rdparty/optional/tomcat.jar"
                     "mcp_3rdparty/optional/masservice.jar"
                     "mcp_3rdparty/optional/stinger.jar"
                     "mcp_3rdparty/core/muse.jar"
                     "mcp_3rdparty/core/commons-logging.jar"
                     "mcp_3rdparty/build/webserv-ext.jar"
                     "mcp_3rdparty/build/webserv-rt.jar"
                     "mcp_3rdparty/optional/commons-beanutils-1.8.0-BETA.jar"
                     "mcp_3rdparty/optional/jsf-api.jar"
                     "mcp_3rdparty/optional/portal-portlet-jsr168api-lib.jar"
                     "mcp_3rdparty/optional/jboss-jmx.jar"
                     "mcp_3rdparty/optional/jbossall-client.jar"
                     "mcp_3rdparty/optional/jboss-jaxrpc.jar"
                     "mcp_3rdparty/optional/portal-api-lib.jar"
                     "mcp_3rdparty/optional/jboss-system.jar"
                     "mcp_3rdparty/optional/portal-common-portal-lib.jar"
                     "mcp_3rdparty/optional/portal-theme-lib.jar"
                     "mcp_3rdparty/build/commons-codec.jar"
                     "mcp_3rdparty/build/jboss-aop-jdk50.jar"
                     "mcp_3rdparty/optional/jboss-jaxws.jar"
                     "mcp_3rdparty/optional/jaxb-api.jar"
                     "mcp_3rdparty/core/3rdParty.jar"
                     "mcp_3rdparty/build/httpservlet-event.jar"
                     "mcp_3rdparty/build/httpservlet-ra.jar"
                     "mcp_3rdparty/build/httpservlet-ratype.jar"
                     "mcp_3rdparty/build/mobicents_lib.jar"
                     "mcp_3rdparty/build/sip-ra.jar"
                     "mcp_3rdparty/build/sip-ratype.jar"
                     "mcp_3rdparty/build/sip-event.jar"
                     "mcp_3rdparty/build/jain-sip-api-1.2.1.jar"
                     "mcp_3rdparty/build/jain-sip-ri-1.2.1.jar"
                     "mcp_3rdparty/build/slee_1_1.jar"
                     "mcp_3rdparty/optional/OPIStubs.jar"
                     "mcp_3rdparty/optional/jsf-impl.jar"
                     "mcp_3rdparty/optional/jbosssx.jar"
                     "mcp_3rdparty/optional/jboss-deployment.jar"
                     "mcp_3rdparty/build/jgroups.jar"
                     "mcp_3rdparty/build/jboss-cache-jdk50.jar"
                     "mcp_3rdparty/build/servlet-api.jar"
                     "mcp_3rdparty/build/trove.jar"
                     "mcp_3rdparty/optional/richfaces-api-3.1.4.GA.jar"
                     "mcp_3rdparty/optional/richfaces-ui-3.1.4.GA.jar"
                     "mcp_3rdparty/optional/richfaces-impl-3.1.4.GA.jar"
                     "mcp_3rdparty/optional/portletbridge-api-1.0.0.B1.jar"
                     "mcp_3rdparty/optional/portletbridge-impl-1.0.0.B1.jar"
                     "mcp_3rdparty/optional/portal-web-lib.jar"
                     "mcp_3rdparty/optional/commons-discovery-0.2.jar"
                     "mcp_3rdparty/optional/axis-jboss.jar"
                     "mcp_3rdparty/build/concurrent.jar"
                     "mcp_3rdparty/build/javassist.jar"
                     "mcp_3rdparty/build/jboss-aop-jdk50-client.jar"
                     "mcp_3rdparty/build/jboss-aspect-jdk50-client.jar"
                     "mcp_3rdparty/build/jboss-aspect-library-jdk50.jar"
                     "mcp_3rdparty/build/jboss-common.jar"
                     "mcp_3rdparty/build/jrockit-pluggable-instrumentor.jar"
                     "mcp_3rdparty/build/pluggable-instrumentor.jar"
                     "mcp_3rdparty/build/qdox.jar"
                     "mcp_3rdparty/optional/hibernate3.jar"
                     "mcp_3rdparty/optional/jboss-hibernate.jar"))

(defvar mcp-jdk-reg '(("1.5.0_11" . "/localdisk/jdk1.5.0_11")
                      ("1.6.0_05" . "/localdisk/jdk1.6.0_05")))

(defun mcp-jde-setup (basedir classdir jar-list jdk)
  (require 'jde)
  (let ((classpath (mapcar '(lambda (j) (concat mk-proj-basedir j)) jar-list)))
    (setq jde-compile-option-directory classdir)
    (setq jde-compile-option-sourcepath (list (concat mk-proj-basedir "mcp_core_root/src")))
    (setq jde-global-classpath (push  classdir classpath))
    (setq jde-jdk-registry mcp-jdk-reg)
    (setq jde-jdk (list jdk))))

(project-def "12static"
             '((basedir "/localdisk/viewstore/matthewk_mcp_core_12.0_3_static/mcp/")
               (src-patterns ("*.java" "*.jsp"))
               (ignore-patterns ("*.class" "*.wsdl"))
               (tags-file "/localdisk/viewstore/matthewk_mcp_core_12.0_3_static/mcp/TAGS")
               (git-p t)
               (compile-cmd "mcpant 12static")
               (startup-hook mcp-12static-startup-hook)
               (shutdown-hook nil)))

(defun mcp-12static-startup-hook ()
  (mcp-jde-setup "/localdisk/data/matthewk/ant/matthewk_mcp_core_12.0_3_static/mcp"
                 "/localdisk/data/matthewk/ant/matthewk_mcp_core_12.0_3_static/classes"
                 mcp12-jars
                 "1.6.0_05")
  (find-file "~/proj/geol3/NOTES")
  (find-file "/localdisk/viewstore/matthewk_mcp_core_12.0_3_static/mcp/mcp_core_root/data/db/run/bin/util/restoreConfigData.pl")
  (find-file "/localdisk/viewstore/matthewk_mcp_core_12.0_3_static/mcp/mcp_core_root/data/db/run/bin/util/saveConfigData.pl"))

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
                 "1.6.0_05"))


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

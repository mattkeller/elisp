;;;; projects.el --- my mk-project settings

(require 'mk-project)
(require 'mcp-projects)
(require 'cl)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
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
               (open-files-cache ,(concat homedir ".cl-sip-open-files"))
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
               (open-files-cache ,(concat homedir ".elisp-open-files"))
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


;;; MCP Projects are 'auto' defined based on their mcpant config files.
;;; Only startup and shutdown hooks need to be defined here

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


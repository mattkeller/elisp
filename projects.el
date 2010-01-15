;;;; projects.el --- my mk-project settings

(require 'mk-project)
(require 'mcp-projects)
(require 'cl)

(setq mk-proj-use-ido-selection t)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file-ido)
(global-set-key (kbd "C-c p F") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(defvar homedir (concat (getenv "HOME") "/"))
(defvar projdir (concat homedir ".mk-project/"))

;;; --------------------------------------------------------------------
;;; cl-sip
;;; --------------------------------------------------------------------

(lexical-let ((bd (concat homedir "code/lisp/cl-sip/"))
      (pd (concat projdir "cl-sip/")))
  (project-def "cl-sip"
               `((basedir ,bd)
                 (src-patterns (".lisp" "*.asd"))
                 (ignore-patterns ("*.fasl"))
                 (tags-file ,(concat bd "TAGS"))
                 (open-files-cache ,(concat pd "open-files"))
                 (vcs git)
                 (startup-hook cl-sip-startup-hook)))

  (defun cl-sip-startup-hook ()
    (slime)
    (dolist (file (directory-files bd t "\.lisp$" t))
      (find-file file))))

;;; --------------------------------------------------------------------
;;; elisp
;;; --------------------------------------------------------------------

(let ((pd (concat projdir "elisp/")))
  (project-def "elisp"
               `((basedir ,(concat homedir "elisp/"))
                 (src-patterns ("*.el"))
                 (file-list-cache ,(concat pd "files"))
                 (open-files-cache ,(concat pd "open-files"))
                 (ignore-patterns ("*.elc"))
                 (tags-file ,(concat pd "TAGS"))
                 (vcs git)
                 (startup-hook elisp-startup-hook))))

(defun elisp-startup-hook ()
  (find-file (concat mk-proj-basedir "dotemacs")))

(let ((pd (concat projdir "mk-project/")))
  (project-def "mk-project"
               `((basedir ,(concat homedir "code/mk-project/"))
                 (src-patterns ("*.el"))
                 (ignore-patterns ("*.elc"))
                 (file-list-cache ,(concat pd "files"))
                 (open-files-cache ,(concat pd "open-files"))
                 (tags-file ,(concat pd "TAGS"))
                 (ack-args "-i")
                 (vcs git))))

;;; --------------------------------------------------------------------
;;; sipbotc
;;; --------------------------------------------------------------------

(lexical-let ((bd (concat homedir "code/sipbotc/"))
      (pd (concat projdir "sipbotc/")))
  (project-def "sipbotc"
               `((basedir ,bd)
                 (src-patterns "*.clj")
                 (ignore-patterns ("*.class"))
                 (file-list-cache ,(concat pd "files"))
                 (open-files-cache ,(concat pd "open-files"))
                 (tags-file ,(concat pd "tags"))
                 (ack-args "-i")
                 (vcs git)
                 (startup-hook sipbotc-hook)))

  (defun sipbotc-hook ()
    (defun clojure ()
      (interactive)
      (require 'slime)
      (clojure-project bd
                       (expand-file-name "lib/clojure-1.0.0.jar" bd)
                       (expand-file-name "lib/clojure-contrib.jar" bd)
                       (expand-file-name "build/classes" bd)
                       (mapcar (lambda (f) (expand-file-name f bd))
                               (list "src" "build/classes" "lib"))))
    (find-file (concat bd "src/com/nortelnetworks/sipbotc/sipbotc.clj"))))

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

(let ((pd (concat projdir "scplite.mcs/")))
      (project-def "scplite.mcs"
                   `((basedir "/home/matthewk/git/scplite.mcs.git")
                     (src-patterns ("*.C" "*.H" "*.c" "*.h"))
                     (ignore-patterns ("*.o"))
                     (tags-file "/home/matthewk/git/scplite.mcs.git/TAGS")
                     (file-list-cache ,(concat pd "files"))
                     (compile-cmd "./buildphx.sh")
                     (vcs git))))


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

;;; --------------------------------------------------------------------
;;; SOA SM Views
;;; --------------------------------------------------------------------

(defun soasm-project-def (name static-p)
  (let* ((bd (if static-p (concat "C:/cc-views/" name)
                          (concat "M:/" name)))         
         (pd (concat "C:/My Documents/mk-project/" name "/")))
    (unless (file-readable-p pd)
      (make-directory pd))
    (if (and static-p (not (file-readable-p bd)))
        (message "Warning: static clearcase view %s does not exist" bd))
    (message "Defining soasm project %s at basedir %s" name bd)
    (project-def name
                 `((basedir ,bd)
                   (src-patterns ("*.java$" "*.xml$" "*.properties$"))
                   (ignore-patterns ("*.wsdl" "*.class" "*.obj" "*.o" ".so" 
                                     ,(concat bd "/devel/thirdparty")))
                   (tags-file ,(concat pd "TAGS"))
                   (open-files-cache ,(concat pd "open-files"))
                   (file-list-cache ,(concat pd "file-list-cache"))
                   (ack-args "--java")
                   (startup-hook soasm-startup-hook)
                   (shutdown-hook soasm-shutdown-hook)))))

(defun soasm-startup-hook ()
  (global-set-key (kbd "C-c p t") 'soasm-tags))

(defun soasm-shutdown-hook
  (global-set-key (kbd "C-c p t") 'project-tags))

(when (string-equal system-type "windows-nt")
  (mapcar (lambda (n) (soasm-project-def n nil)) '("kelma12-r12.5"))
  (mapcar (lambda (n) (soasm-project-def n t)) '()))

;;; --------------------------------------------------------------------
;;; SOASM Utils
;;; --------------------------------------------------------------------

(defun soasm-tags () 
  (interactive)
  (mk-proj-assert-proj)
  (if mk-proj-tags-file
      (let* ((tags-name (file-name-nondirectory mk-proj-tags-file))
             (tags-dir (file-name-directory mk-proj-tags-file))
             (dev-dir (concat mk-proj-basedir "/devel"))
             (not-dev-dir (concat dev-dir "/thirdparty"))
             (custom-find-cmd (concat "find '" dev-dir "' -path '" not-dev-dir "' -prune -o "
                                      "\\( -name '*.java' -type f -print \\) "
                                      "| etags -l java -o '" tags-name "' - "))
             (default-directory tags-dir)
             (proc-name "etags-process"))
        (message "Refreshing TAGS file %s..." mk-proj-tags-file)
        (start-process-shell-command proc-name "*etags*" custom-find-cmd)
        (set-process-sentinel (get-process proc-name) 'mk-proj-etags-cb))
    (message "mk-proj-tags-file is not set")))

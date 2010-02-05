;;;; projects.el --- my mk-project settings

(require 'mk-project)
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
;;; SOA SM Views
;;; --------------------------------------------------------------------

(defun soasm-project-def (name static-p)
  (let* ((bd (if static-p (concat "C:/cc-views/" name)
                          (concat "M:/" name)))         
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
                   (startup-hook nil)
                   (shutdown-hook nil)))))

(defun soasm-find-cmd (context)
  (assert mk-proj-basedir)
  (let* ((dev-dir      (concat mk-proj-basedir "/devel"))
         (prune-clause (concat "\\( -path '" dev-dir "/thirdparty' -prune \\)"))
         (src-clause   "\\( -type f \\( -name '*.cpp' -o -name '*.[cChH]' -o -name '*.java' \\) -print \\)"))
    (ecase context
      ('src   (concat "find '" dev-dir "' " prune-clause " -o " src-clause))
      ('index (concat "find '" dev-dir "' " prune-clause " -o -print"))
      ('grep  (if (string= (expand-file-name default-directory) mk-proj-basedir)
                  ; custom find cmd
                  (replace-regexp-in-string "print" "print0" (soasm-find-cmd 'src))
                ; default grep args
                nil)))))

(when (string-equal system-type "windows-nt")
  (mapcar (lambda (n) (soasm-project-def n nil)) '("kelma12-r12.5"))
  (mapcar (lambda (n) (soasm-project-def n t)) '("kelma12-r12.5-soa-svc")))

;;; --------------------------------------------------------------------
;;; SOASM Utils
;;; --------------------------------------------------------------------

(defun soasm-build ()
  "Write and run a bat file to compile our SOA view"
  (interactive)
  (mk-proj-assert-proj)
  (let* ((build-script (expand-file-name "~/soa-build.bat"))
         (basedir-tmp (shell-command-to-string (concat "cygpath -w '" mk-proj-basedir "'")))
         (basedir (substring basedir-tmp 0 (- (length basedir-tmp) 1)))
         (develdir (concat basedir "\\devel"))
         (go-home (concat "cd " develdir "\n"))
         (static-view-p (string-match "cc-views" basedir)))
    
    ;; write our build script to disk
    (with-temp-file build-script
      (if (not static-view-p)
          (insert (concat
               "call set PS_BUILD_VIEW=" basedir "\n"
               "call set PS_VIEW_PREFIX=%PS_BUILD_VIEW%\\devel\n"
               "call set WA_BUILD_VIEW=" basedir "\n"
               "call set WA_VIEW_PREFIX=%WA_BUILD_VIEW%\\devel\n"
               "call set TM_BUILD_VIEW=" basedir "\n"
               "call set TM_VIEW_PREFIX=%TM_BUILD_VIEW%\\devel\n"
               "\n")))
      (insert go-home)
      (insert (concat
               "cd tools\\bin\n"
               "call buildenv\n"))
      (insert go-home)
      (dolist (d ;'("common" "sdk" "xps" "policy-server" "agentcommon"
                 ;  "agentframework" "affl-minder/policy-server"
                   '("transactionminder"))
        (insert (concat "cd " d "\ncall clearmake -VF\n" go-home)))
      (dolist (d '("app-server\was\scripts" "app-server\sspi\scripts"))
        (insert (concat "cd " d "\ncall build-all.cmd\n" go-home)))
      (dolist (d '("soamgr"))
        (insert (concat "cd " d "\ncall clearmake -VF\n" go-home))))
    (assert (file-readable-p build-script))

    ;; run the build script in compile mode
    ;; TODO: issues cmd before the shell is ready? Just sits at the prompt...
    (let* ((shell-file-name "cmd.exe")
           (explicit-shell-file-name shell-file-name)
           (default-directory (file-name-directory build-script))
           (bat-script (file-name-nondirectory build-script)))
      (compile bat-script t))))
    


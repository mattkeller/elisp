;;;; host-mktop.el -- setup for my laptop

(require 'mk-project)
(require 'cl)

;;; cl-sip -------------------------------------------------------------

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

;;; mk-project ---------------------------------------------------------

(let ((pd (concat projdir "mk-project/")))
  (project-def "mk-project"
               `((basedir ,(concat homedir "code/mk-project"))
                 (src-patterns ("*.el"))
                 (ignore-patterns ("*.elc"))
                 (file-list-cache ,(concat pd "files"))
                 (open-files-cache ,(concat pd "open-files"))
                 (tags-file ,(concat pd "TAGS"))
                 (ack-args "-i")
                 (vcs git))))

;;; sipbotc ------------------------------------------------------------

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

;;; Qrev ---------------------------------------------------------------

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

;;; compojure ----------------------------------------------------------

(project-def "compojure"
             `((basedir "~/code/compojure")
               (src-patterns ("*.clj" "*.java"))
               (ignore-patterns ("*.class"))
               (tags-file "~/code/compojure/TAGS")
               (file-list-cache "~/code/compojure/.file-index")
               (vcs git)
               (compile-cmd "ant")))


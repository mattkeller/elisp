;;;; host-kelma12755.el -- host-specific config

(require 'mk-project)
(require 'cl)

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

;;; Javadoc-help --------------------------------------------------------

(require 'javadoc-help)

;; What an ugly hack this is! This pkg needs some love.
(let ((refreshed t)
      (enabled t)
      (predefined t))
  (setq *jdh-javadocs* (mapcar (lambda (url) (jdh-javadoc-new url refreshed enabled predefined))
                               '("c:/Documents and Settings/kelma12/My Documents/apidocs/jdk1.6.0-api"
                                 "c:/Program Files/Java/Glassfish/glassfish/docs/api"))))

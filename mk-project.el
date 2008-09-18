;;;; mk-project.el ---  Lightweight project handling
;;;;
;;;; Perform operations -- find-grep, compile, find-file, visit-tags-file --
;;;; on a per-project basis. Projects are defined by passing project-def
;;;; a list of settings. Project commands use these settings to restrict
;;;; or enhance their operation. These operations allow IDE-like behavior,
;;;; quick project setup and switching.
;;;;
;;;; Project Administration:
;;;;   * Load project:    project-load
;;;;   * Unload project:  project-unload
;;;;   * Project Status:  project-status
;;;;   * Close files:     project-close-files
;;;;
;;;; Project Operations:
;;;;   * Compile project: project-compile
;;;;   * Grep project:    project-grep
;;;;   * Find file:       project-find-file
;;;;   * Index files:     project-index
;;;;   * Cd proj home:    project-home
;;;;   * Rebuild tags:    project-tags
;;;;   * Open Dired:      project-dired
;;;;
;;;; Example use:
;;;;   (project-def "my-proj"
;;;;         '((basedir "/home/me/my-proj/")
;;;;           (src-patterns ("*.lisp" "*.c"))
;;;;           (ignore-patterns ("*.elc" "*.o"))
;;;;           (tags-file "/home/me/my-proj/TAGS")
;;;;           (git-p t)
;;;;           (compile-cmd "make")
;;;;           (startup-hook myproj-startup-hook)))
;;;;
;;;;   (defun myproj-startup-hook () 
;;;;     (find-file "/home/me/my-proj/foo.el"))
;;;;

(require 'grep)
(require 'thingatpt)

;; ---------------------------------------------------------------------
;; Utils
;; ---------------------------------------------------------------------

(defmacro aif (test true-body &rest false-body)
  "Evaluate TRUE-BODY or FALSE-BODY depending on value of TEST.
If TEST returns non-nil, bind `it' to the value, and evaluate
TRUE-BODY.  Otherwise, evaluate forms in FALSE-BODY as if in `progn'.
Compare with `if'."
  (let ((sym (gensym "--ibuffer-aif-")))
    `(let ((,sym ,test))
       (if ,sym
	   (let ((it ,sym))
	     ,true-body)
	 (progn
	   ,@false-body)))))

(defun mk-proj-replace-tail (str tail-str replacement)
  (if (string-match (concat tail-str "$")  str)
    (replace-match replacement t t str)
    str))

;; ---------------------------------------------------------------------
;; Project Configuration
;; ---------------------------------------------------------------------

(defvar mk-proj-list (make-hash-table :test 'equal))

(defun mk-proj-find-config (proj-name)
  (gethash proj-name mk-proj-list))

(defun project-def (proj-name config-alist)
  "Assciate the settings in the alist <config-alist> with project <proj-name>"
  (puthash proj-name config-alist mk-proj-list))

(defvar mk-proj-name nil "Name of the current project.")
(defvar mk-proj-basedir (getenv "HOME") "Base directory of the current project.")
(defvar mk-proj-src-patterns nil "List of shell expressions to search with grep-find, eg: '(\"*.java\" \"*.jsp\".)")
(defvar mk-proj-ignore-patterns nil "List of shell expressions to avoid searching for with project-find-file, eg '(\"*.class\").")
(defvar mk-proj-git-p nil "True if this is a git project. Project commands will avoid the .git directory.")
(defvar mk-proj-tags-file nil "Path to the TAGS file for this project.")
(defvar mk-proj-compile-cmd nil "Command to build the entire project.")
(defvar mk-proj-startup-hook nil "Hook function to run after project-load.")
(defvar mk-proj-shutdown-hook nil "Hook function to run afer project-unload.")

(defun mk-proj-defaults ()
  "Set all default values for vars and keybindings"
  (setq mk-proj-name nil
        mk-proj-basedir (getenv "HOME")
        mk-proj-src-patterns nil
        mk-proj-ignore-patterns nil
        mk-proj-git-p nil
        mk-proj-tags-file nil
        mk-proj-compile-cmd "make -k"
        mk-proj-startup-hook nil
        mk-proj-shutdown-hook nil)
  (cd mk-proj-basedir))

(defun mk-proj-config-val (key config-alist)
  "Get a config value from a config alist, nil if doesn't exist"
  (if (assoc key config-alist)
    (car (cdr (assoc key config-alist)))
    nil))

(defun mk-proj-load-vars (proj-name proj-alist)
  "Set vars from config alist"
  (mk-proj-defaults)
  ;; required vars
  (setq mk-proj-name proj-name)
  (setq mk-proj-basedir (expand-file-name (mk-proj-config-val 'basedir proj-alist)))
  ;; optional vars
  (aif (mk-proj-config-val 'src-patterns proj-alist) (setq mk-proj-src-patterns it))
  (aif (mk-proj-config-val 'ignore-patterns proj-alist) (setq mk-proj-ignore-patterns it))
  (aif (mk-proj-config-val 'git-p proj-alist) (setq mk-proj-git-p it))
  (aif (mk-proj-config-val 'tags-file proj-alist) (setq mk-proj-tags-file (expand-file-name it)))
  (aif (mk-proj-config-val 'compile-cmd proj-alist) (setq mk-proj-compile-cmd it))
  (aif (mk-proj-config-val 'startup-hook proj-alist) (setq mk-proj-startup-hook it))
  (aif (mk-proj-config-val 'shutdown-hook proj-alist) (setq mk-proj-shutdown-hook it)))

(defun project-load ()
  "Load a project's settings."
  (interactive)
  (catch 'project-load
    (let ((oldname mk-proj-name)
          (name (completing-read "Project Name: " mk-proj-list)))
      (unless (string= oldname name)
        (project-unload))
      (aif (mk-proj-find-config name)
           (mk-proj-load-vars name it)
           (message "Project %s does not exist!" name)
           (throw 'project-load t))
      (when (not (file-directory-p mk-proj-basedir))
        (message "Base directory %s does not exist!" mk-proj-basedir)
        (throw 'project-load t))
      (message "Loading project %s" name)
      (cd mk-proj-basedir)
      (when mk-proj-tags-file 
        (aif (get-buffer "TAGS") (kill-buffer it))
        (visit-tags-table mk-proj-tags-file))
      (project-index)
      (when mk-proj-startup-hook
        (run-hooks 'mk-proj-startup-hook)))))

(defun project-unload ()
  "Revert to default project settings."
  (interactive)
  (when mk-proj-name
    (message "Unloading project %s" mk-proj-name)
    (when mk-proj-tags-file (aif (get-buffer "TAGS") (kill-buffer it)))
    (mk-proj-fib-clear)
    (when (and (mk-proj-buffers)
               (y-or-n-p (concat "Close all " mk-proj-name " project files? "))
      (project-close-files)))
    (when mk-proj-shutdown-hook (run-hooks 'mk-proj-shutdown-hook)))
  (mk-proj-defaults)
  (message "Project settings have been cleared"))

(defun project-close-files ()
  "Close all unmodified files that reside in the project's basedir"
  (let ((closed nil)
        (dirty nil)
        (basedir-len (length mk-proj-basedir)))
    (dolist (b (mk-proj-buffers))
      (cond
       ((buffer-modified-p b)
        (push (buffer-name) dirty))
       (t
        (kill-buffer b)
        (push (buffer-name) closed))))
    (message "Closed %d buffers, %d modified buffers where left open" 
             (length closed) (length dirty))))

(defun mk-proj-buffer-p (buf)
  "Is the given buffer in our project  based on filename? Also detects dired buffers open to basedir/*"
  (let ((file-name (buffer-file-name buf)))
    (if (and file-name
             (string-match (concat "^" (regexp-quote mk-proj-basedir)) file-name))
        t
      nil)))

(defun mk-proj-buffers ()
  "Get a list of buffers that reside in this project's basedir"
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj-buffer-p b) (push b buffers)))
    buffers))

(defun project-status ()
  "View project's variables."
  (interactive)
  (message "Name=%s; Basedir=%s; Src=%s; Ignore=%s; Git-p=%s; Tags=%s; Compile=%s; Startup=%s; Shutdown=%s"
           mk-proj-name mk-proj-basedir mk-proj-src-patterns mk-proj-ignore-patterns mk-proj-git-p
           mk-proj-tags-file mk-proj-compile-cmd mk-proj-startup-hook mk-proj-shutdown-hook))

;; ---------------------------------------------------------------------
;; Etags
;; ---------------------------------------------------------------------

(defun mk-proj-etags-cb (process event)
  "Visit tags table when the etags process finishes."
  (message "Etags process %s received event %s" process event)
  (when (string= event "finished\n")
    (visit-tags-table mk-proj-tags-file)))

(defun project-tags ()
  "Regenerate the projects TAG file. Runs in the background."
  (interactive)
  (if mk-proj-tags-file
    (progn
      (cd mk-proj-basedir)
      (message "Refreshing TAGS file %s..." mk-proj-tags-file)
      (let ((etags-cmd (concat "find " mk-proj-basedir " -type f " 
                               (mk-proj-find-cmd-src-args mk-proj-src-patterns)
                               " | etags -o " mk-proj-tags-file " - "))
            (proc-name "etags-process"))
        (start-process-shell-command proc-name "*etags*" etags-cmd)
        (set-process-sentinel (get-process proc-name) 'mk-proj-etags-cb)))
    (message "mk-proj-tags-file is not set")))

(defun mk-proj-find-cmd-src-args (src-patterns)
  "Generate the ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (let ((name-expr " \\("))
    (dolist (pat src-patterns)
      (setq name-expr (concat name-expr " -name \"" pat "\" -o ")))
    (concat (mk-proj-replace-tail name-expr "-o " "") "\\) ")))

(defun mk-proj-find-cmd-ignore-args (ignore-patterns)
  "Generate the -not ( -name <pat1> -o -name <pat2> ...) pattern for find cmd"
  (concat " -not " (mk-proj-find-cmd-src-args ignore-patterns)))

;; ---------------------------------------------------------------------
;; Grep 
;; ---------------------------------------------------------------------

(defun project-grep ()
  "Run find-grep using this project's settings for basedir and src files."
  (interactive)
  (let* ((wap (word-at-point))
         (regex (if wap (read-string (concat "Grep project for (default \"" wap "\"): ") nil nil wap)
                 (read-string "Grep project for: "))))
    (cd mk-proj-basedir)
    (let ((find-cmd (concat "find . -type f"))
          (grep-cmd (concat "grep -i -n \"" regex "\"")))
      (when mk-proj-src-patterns
        (setq find-cmd (concat find-cmd (mk-proj-find-cmd-src-args mk-proj-src-patterns))))
      (when mk-proj-tags-file
        (setq find-cmd (concat find-cmd " -not -name 'TAGS'")))
      (when mk-proj-git-p
        (setq find-cmd (concat find-cmd " -not -path '*/.git*'")))
      (grep-find (concat find-cmd " -print0 | xargs -0 -e " grep-cmd)))))

;; ---------------------------------------------------------------------
;; Compile 
;; ---------------------------------------------------------------------

(defun project-compile (opts)
  "Run the compile command for this project."
  (interactive "sCompile options: ")
  (project-home)
  (compile (concat mk-proj-compile-cmd " " opts)))

;; ---------------------------------------------------------------------
;; Home and Dired
;; ---------------------------------------------------------------------

(defun project-home ()
  "cd to the basedir of the current project"
  (interactive)
  (cd mk-proj-basedir))

(defun project-dired ()
  "Open dired in the project's basedir (or jump to the existing dired buffer)"
  (interactive)
  (dired mk-proj-basedir))

;; ---------------------------------------------------------------------
;; Find-file 
;; ---------------------------------------------------------------------

(defconst mk-proj-fib-name "*file-index*")

(defun mk-proj-fib-clear ()
  "Clear the contents of the fib buffer"
  (aif (get-buffer mk-proj-fib-name)
    (with-current-buffer it
      (setq buffer-read-only nil)
      (kill-region (point-min) (point-max))
      (setq buffer-read-only t))))

(defun mk-proj-fib-cb (process event)
  "Handle failure to complete fib building"
  (cond
   ((string= event "finished\n")
    (with-current-buffer (get-buffer mk-proj-fib-name) 
      (setq buffer-read-only t))
    (message "Refreshing %s buffer...done" mk-proj-fib-name))
   (t
    (mk-proj-fib-clear)
    (message "Failed to generate the %s buffer!" mk-proj-fib-name))))

(defun project-index ()
  "Regenerate the *file-index* buffer that is used for project-find-file"
  (interactive)
  (message "Refreshing %s buffer..." mk-proj-fib-name)
  (mk-proj-fib-clear)
  (let ((find-cmd (concat "find " mk-proj-basedir " -type f " 
                          (mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns)))
        (proc-name "index-process"))
    (when mk-proj-git-p
      (setq find-cmd (concat find-cmd " -not -path '*/.git*'")))
    (with-current-buffer (get-buffer-create mk-proj-fib-name)
      (buffer-disable-undo) ;; this is a large change we don't need to undo
      (setq buffer-read-only nil))
    (start-process-shell-command proc-name mk-proj-fib-name find-cmd) 
    (set-process-sentinel (get-process proc-name) 'mk-proj-fib-cb)))

(defun* project-find-file (regex)
  "Find file in the current project matching the given regex.

The file list in buffer *file-index* is scanned for regex matches. If only
one match is found, the file is opened automatically. If more than one match
is found, this prompts for completion. See also: project-index."
  (interactive "sFind file in project matching: ")
  (unless (get-buffer mk-proj-fib-name)
    (when (yes-or-no-p "No file index exists for this project. Generate one? ")
      (project-index))
    (message "Cancelling project-find-file")
    (return-from "project-find-file" nil))
  (with-current-buffer mk-proj-fib-name
    (let ((matches nil))
      (goto-char (point-min))
      (dotimes (i (count-lines (point-min) (point-max)))
        (let ((bufline (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match regex bufline)
            (push bufline matches))
          (forward-line)))
      (let ((match-cnt (length matches)))
        (cond
         ((= 0 match-cnt)
          (message "No matches for \"%s\" in this project" regex))
         ((= 1 match-cnt )
          (find-file (car matches)))
         (t
          (let ((file (completing-read "Multiple matches, pick one: " matches)))
            (when file
              (find-file file)))))))))

(provide 'mk-project)

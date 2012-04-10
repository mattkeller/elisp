;;;; java.el -- java coding config

(add-to-list 'load-path (concat dotfiles-dir "lib/elib-1.0"))
(add-to-list 'load-path (concat dotfiles-dir "lib/cedet-1.0pre6/common/"))
(add-to-list 'load-path (concat dotfiles-dir "lib/jdee-2.4.0/lisp"))

(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory   "~/.semantic.cache"
      semanticdb-persistent-path          nil)

;; Load JDE (and prereqs, like cedet) when we first load a java file,
;; not on startup
(autoload 'jde-mode "jde" "JDE mode." t)
(add-to-list 'auto-mode-alist '("\\.java$" . jde-mode))

(when (getenv "JAVA_HOME")
  (let* ((path (getenv "JAVA_HOME"))
         (version (file-name-nondirectory path)))
    (setq jde-jdk-registry `((,version . ,(getenv "JAVA_HOME")))
          jde-jdk           version)))

(add-hook 'jde-mode-hook '(lambda () 
                            (require 'jde-ant)
                            (message "jde-mode time")
                            (local-set-key [f5] 'jde-compile)))


(add-hook 'java-mode-hook '(lambda ()
                             (c-set-style "java")
                             (setq c-basic-offset 4)
                             (c-set-offset 'substatement-open 0)
                             (setq-default indent-tabs-mode nil
                                           tab-width 4)
                             (mk-maybe-hide-imports)
                             (subword-mode)
                             (mk-coding-hook)
                             (local-set-key (kbd "C-.") 'javadoc-lookup)))

(defun mk-maybe-hide-imports ()
  (save-excursion
    (goto-char (point-min))
    (when (>= (count-matches "^import ") 5)
    (message "Lotsa import statements, let's hide them")
    (mk-hide-java-imports))))

(defun find-class-in-jars ()
  "Find the jar file with this class in it. Drops output to *jars* buffer"
  (require 'find-cmd)
  (interactive)
  (let* ((def-dir (if mk-proj-basedir mk-proj-basedir default-directory))
         (start-dir (ido-read-directory-name "Start Directory: " def-dir def-dir))
         (class-regex (read-string "Class: ")) ; default to thing at point or class of current file
         (3p (if (file-exists-p (concat start-dir "thirdparty"))
                 (y-or-n-p "Search 3rd party jars?")))
         (3p-clause (if (not 3p) (concat "\\( -path '" start-dir "thirdparty'" " -prune \\) -o " "")))
         (find-cmd (concat "find \"" start-dir "\" " 3p-clause " -type f \\( -name '*.jar' -o -name '*.war' -o -name '*.ear' \\) -print0 | "
                           "xargs -n1 -0i sh -c 'jar tf \"{}\" | "
                           "grep -i -q " class-regex " && echo \"{}\"' 2> /dev/null"))
         (proc-name "find-class-in-jars")
         (buf (generate-new-buffer "*jars*")))
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (insert (concat "Looking for \"" class-regex "\" in all jar files under " start-dir ":\n\n"))
      (insert (concat "DEBUG: " find-cmd "\n\n")))
    (message "Started jar search ...")
    (start-process-shell-command proc-name buf find-cmd)
    (set-process-sentinel (get-process proc-name) 'find-class-in-jars-cb)))

(defun find-class-in-jars-cb (process event)
  (with-current-buffer (get-buffer "*jars*")
    (insert "\nDone.\n\n"))
    (message "Started jar search ... done. See *jars*"))

(defun ant-call (target)
  "Call Ant's build.xml"
  (interactive "MAnt Target: ")
  (let ((oldbuf (get-buffer "*ant-compilation*")))
    (if (not (null oldbuf))
      (kill-buffer "*ant-compilation*")))
  (let* ((buildfile (file-search-upward (file-name-directory (buffer-file-name)) "build.xml"))
	 (outbuf (get-buffer-create "*ant-compilation*"))
	 (curbuf (current-buffer)))
    (switch-to-buffer-other-window outbuf)
    (insert "#> ant -emacs -f " buildfile " " target "\n")
    (switch-to-buffer-other-window curbuf)
    (call-process "ant" nil outbuf t "-emacs" "-f" buildfile target)))

(provide 'java)

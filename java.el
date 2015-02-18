;;;; java.el -- java coding config

(require 'mk-utils)

;;; Javadoc-Lookup --------------------------------------------------------

(add-to-list 'load-path (concat dotfiles-dir "lib/javadoc-lookup"))
(require 'javadoc-lookup)
(global-set-key (kbd "C-h j") 'javadoc-lookup)

;;; cedet -----------------------------------------------------------------

(add-to-list 'load-path (concat dotfiles-dir "lib/elib-1.0"))
(add-to-list 'load-path (concat dotfiles-dir "lib/cedet-1.0pre6/common/"))

(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory   "~/.semantic.cache"
      semanticdb-persistent-path          nil)

(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

(when (getenv "JAVA_HOME")
  (let* ((path (getenv "JAVA_HOME"))
         (version (file-name-nondirectory path)))
    (setq jde-jdk-registry `((,version . ,(getenv "JAVA_HOME")))
          jde-jdk           version)))

(add-hook 'java-mode-hook '(lambda ()
                             (mk-coding-hook)
                             (c-set-style "java")
                             (setq c-basic-offset 4)
                             (c-set-offset 'substatement-open 0)
                             (setq-default indent-tabs-mode nil
                                           tab-width 4)
                             (mk-maybe-hide-imports)
                             (mk-coding-hook)
                             (local-set-key (kbd "C-.") 'javadoc-lookup)))

(defun mk-maybe-hide-imports ()
  (save-excursion
    (goto-char (point-min))
    (when (>= (count-matches "^import ") 5)
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

(defun file-search-upward (directory file)
  "search a file upward"
  (let* ((updir (file-truename (concat (file-name-directory directory) "../")))
         (curfd (if (not (string= (substring directory (- (length directory) 1)) "/"))
                    (concat directory "/" file)
                  (concat directory file))))
    (if (file-exists-p curfd)
        curfd
      (if (and (not (string= (file-truename directory) updir))
               (< (length updir) (length (file-truename directory))))
          (file-search-upward updir file)
        nil))))

(defconst java-import-regexp "^import")
(defconst java-blank-line-regexp "[ \t]*$")
(defconst java-import-class-regexp "\\(^import[ \t]+\\([a-zA-Z0-9_]+\\.\\)+\\)\\([A-Z][a-zA-Z0-9_]*\\);")

(defun java-sort-imports ()
  (interactive "")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward java-import-regexp nil t)
        (progn
          (beginning-of-line)
          (let ((start (point))
                end
                last-import-line)

            ;; bracket the import statements, deleting any blank lines
            (while (or (looking-at java-import-regexp)
                       (looking-at java-blank-line-regexp))
              (if (looking-at java-blank-line-regexp)
                  (kill-line)
                (beginning-of-line 2)))
            (setq end (point))

            ;; sort the imports and restore the blank line after the last one.
            ;; TODO don't include "static" in the sort
            (sort-lines nil start end)
            (open-line 1)

            ;; Walk through the imports, separate them by package
            ;; name. Comment out the unused imports.
            (goto-char start)
            (while (looking-at java-import-class-regexp)
              (let ((line (trim-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                    (package (match-string 1))
                    (class (match-string 3)))
                (when (and (not (null last-import-line))
                           (string= last-import-line line))
                  (kill-line)
                  (delete-backward-char 1)
                  (beginning-of-line))
                (setq last-import-line line)
                (when (null (save-excursion
                              (beginning-of-line 2)
                              (word-search-forward class nil t)))
                  (insert "// UNUSED "))
                (beginning-of-line 2)
                (if (null (looking-at package))
                    (newline-and-indent)))))))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

;;; Groovy -------------------------------------------------------------

(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(defun groovy-run ()
  "Run this groovy script"
  (interactive)
  (shell-command (concat "groovy \"" (buffer-file-name) "\"")))

(add-hook 'groovy-mode-hook
          '(lambda ()
             (local-set-key [f5] 'groovy-run)
             (linum-mode)))

(provide 'java)

;;;; TAGS utils ----------------------------------------------------------

;; Variables tags-file-name and tags-table-list are mutually
;; exclusive. We'll use the more general solution - the list.  The
;; files in the list are not loaded until you try to find a tag.

;; In visit-tags-table, don't prompt to ask if we should open a 2nd or 3rd tags file
(setq tags-add-tables nil)

(defun mktags/show-tags-files ()
  "Show which tags files are configured"
  (interactive)
  (describe-variable 'tags-table-list))

(defun mktags/clear-tags ()
  "Resets lists of tags files and deletes associated tags buffers"
  (interactive)
 
  ;; clear tags-table-list and buffers
  (dolist (f tags-table-list)
    (let ((b (get-file-buffer f)))
      (when b
        (kill-buffer b))))
  (setq tags-table-list nil)

  ;; clear tags-file-name and buffer
  (when tags-file-name
    (let ((b (get-file-buffer tags-file-name)))
      (when b
        (kill-buffer b)))
    (setq tags-file-name nil)))

(defun mktags/add-tags-file (file)
  "Add a tags file to tags-table-list"
  (interactive "fTAGS file: ")
  (let ((full-name (expand-file-name file)))
    (when (not (member full-name tags-table-list))
      (when (file-readable-p full-name)
        (setq tags-file-name nil)
        (add-to-list 'tags-table-list full-name)
        (message "Added %s to tags list" full-name)))))

(defun mktags/load-tags-in-dir (dir)
  "Add all \"TAGS.*\" files in a given directory to tags-table-list"
  (interactive "Ddir: ")
  (let* ((tags-files (directory-files (file-name-as-directory dir) t "^TAGS")))
    (dolist (f tags-files)
      (mktags/add-tags-file f))
    (message "Tags files: %s" tags-table-list)))
        
(defun mktags/load-project-tags ()
  "Load all \"TAGS.*\" files in the mk-project basedir"
  (interactive)
  (when mk-proj-basedir
    (mktags/load-tags-in-dir mk-proj-basedir)))

(defun mktags/tags-for-dir (dir)
  "Run etags to create a new \"TAGS.<dir>\" file, then load the TAGS file"
  (interactive "Ddir: ")
  (let* ((dir-name (file-name-nondirectory (directory-file-name dir)))
         (parent-dir (file-name-as-directory (expand-file-name (concat (file-name-as-directory dir) ".."))))
         (tags-file (concat parent-dir "TAGS." dir-name))
         (default-directory parent-dir)
         (find-cmd (concat "find '" dir-name "' -name \"*.cpp\" -o -name \"*.[cChH]\" -o -name \"*.java\""))
         (etags-cmd (concat "etags -o '" tags-file "' - "))
         (cmd (concat find-cmd " | " etags-cmd))
         (proc-name "mk-tags-for-dir")
         (proc (start-process-shell-command proc-name "*etags*" cmd)))
    (message "Building tags-file: %s; cmd: %s" tags-file cmd)
    (process-put proc 'tags-file tags-file) ; no closures in elisp, sigh.
    (set-process-sentinel proc (lambda (proc event)
                                 (let ((tags-file (process-get proc 'tags-file)))
                                   (message (concat "Process " (process-name proc) " finished with event '" event "'; tags-file " tags-file))
                                   (cond
                                    ((string= "finished\n" event)
                                     (when (file-readable-p tags-file)
                                       (mktags/add-tags-file tags-file)
                                       (message "Refreshing TAGS file %s...done" tags-file)))
                                    (t (message "Refreshing TAGS file %s...failed" tags-file))))))))

(defun mktags/tags-for-subdirs (base-dir relative-dirs)
  "Build a TAGS file for each 'relative-dir' (relative to 'base-dir')"
  (let ((dir-as-dir (file-name-as-directory base-dir)))
    (when (file-exists-p dir-as-dir)
      (dolist (srcd relative-dirs)
        (when (file-exists-p (concat dir-as-dir srcd))
          (when (not (file-exists-p (concat dir-as-dir "TAGS." srcd)))
            (mktags/tags-for-dir (concat dir-as-dir srcd))))))))

(defun mktags/fed-tags (dir)
  "Build TAGS files for the Federation sources"
  (interactive "Ddir: ")
  (let ((src-dirs '("common" "sdk" "xps" "policy-server" "xps-objects"
                    "agentcommon" "affl-minder" "federation")))
    (mktags/tags-for-subdirs dir src-dirs)))

(defun mktags/soa-tags (dir)
  "Build TAGS files for the SOASM sources"
  (interactive "Ddir: ")
  (let ((src-dirs '("common" "sdk" "xps" "policy-server" "xps-objects"
                    "agentcommon" "agentframework" "transactionminder"
                    "soaagent" "affl-minder/fedcommon" "affl-minder/policy-server")))
    (mktags/tags-for-subdirs dir src-dirs)))
       
(provide 'mk-tags)

;;;; etags-update.el
;;
;; Minor mode to update the TAGS file when a file is saved
;;
;; TODO:
;; - Hook allowing user to decide to add a file to TAGS
;;   programtically.
;; - Don't offer to add a non-code file to TAGS

(defgroup etags-update nil
  "Minor mode to update the TAGS file when a file is saved"
  :group 'tools)

(defcustom etu/append-file-prompt 'prompt
  "Add file not already in TAGS to TAGS?"
  :type '(choice (const always) (const prompt) (const never))
  :group 'etags-update)

(defun etu/tags-file-dir ()
  "Return full directory of the TAGS file"
  (assert (get-file-buffer tags-file-name))
  (with-current-buffer (get-file-buffer tags-file-name)
    (expand-file-name default-directory)))

(defun etu/file-pattern-in-tags-buffer (buffer pattern)
  "Search for `pattern' in a 'file' line of a given TAGS
`buffer'. Return the match or nil"
  (with-current-buffer buffer
    (save-excursion ; useful only for debugging, can remove later
      (goto-char (point-min))
      (let ((match))
        (catch 'loop-exit
          (while (and (search-forward pattern nil t) (not match))
            (beginning-of-line)
            ;; Capture the whole filename on this line. The regex also
            ;; ensures we're on a 'file' line
            (if (re-search-forward "^\\(.*\\),[0-9]+$" nil t)
                (let ((file-in-tags (buffer-substring (match-beginning 1) (match-end 1))))
                  (when (string= pattern file-in-tags)
                    (setq match file-in-tags)))))
          (throw 'loop-exit nil))
        match))))

(defun etu/file-in-tags (file)
  "Given a absolute filename, search for it, or its filename
relative to the TAGS file directory, in the TAGS buffer. Return
the match or nil."
  (assert (file-name-absolute-p file))
  (let* ((tags-buffer (get-file-buffer tags-file-name))
         (tags-dir    (etu/tags-file-dir))
         (file-rel    (substring file (length tags-dir))))
    (or (etu/file-pattern-in-tags-buffer tags-buffer file)
        (and (string= file (concat tags-dir file-rel)) ; ensure file-rel is in tags-dir
             (etu/file-pattern-in-tags-buffer tags-buffer file-rel)))))

(defun etu/test-file-pattern-in-tags-buffer ()
  "Testing utu/file-pattern-in-tags-buffer"
  (with-temp-buffer
    (insert "junkline\n")
    (insert "/home/mk/foo,10\n")
    (insert "bar,10\n")
    (insert "/home/mk/abcdefg,10\n")
    (assert (string= "/home/mk/foo" (etu/file-pattern-in-tags-buffer (current-buffer) "/home/mk/foo")))
    (assert (string= "bar" (etu/file-pattern-in-tags-buffer (current-buffer) "bar")))
    (assert (string= "/home/mk/abcdefg" (etu/file-pattern-in-tags-buffer (current-buffer) "/home/mk/abcdefg")))
    (assert (null (etu/file-pattern-in-tags-buffer (current-buffer) "/home/mk/abc")))
    (assert (null (etu/file-pattern-in-tags-buffer (current-buffer) "bcd")))
    (assert (null (etu/file-pattern-in-tags-buffer (current-buffer) "mk/abcdefg")))
    (assert (null (etu/file-pattern-in-tags-buffer (current-buffer) "junkline")))))

(defun etu/update-cb (process event)
  "Callback fn to handle etags-update.pl termination"
  (cond
   ((string= event "finished\n")
    ;; Manualy re-visit TAGS if we appended a new file -- we might not
    ;; have use find-tags between saves and we don't want to re-prompt
    ;; to add the file.
    (visit-tags-table (expand-file-name tags-file-name))
    (message "Refreshing TAGS file ...done")
    (when (get-buffer  "*etags-update*")
      (kill-buffer (get-buffer "*etags-update*"))))
   (t (message "Refreshing TAGS file failed. Event was %s. See buffer *etags-update*." event))))

(defun etu/update-for-file ()
  "Update the TAGS file for the file of the current buffer. If
the file is not already in TAGS, maybe add it."
  (interactive)
  (catch 'etu/update-for-file
    (when tags-file-name
      (let* ((file              (buffer-file-name (current-buffer)))
             (file-in-tags      (etu/file-in-tags file))
             (cmd               (concat "etags-update.pl " tags-file-name " " file-in-tags))
             (proc-name         "etags-update")
             (default-directory (etu/tags-file-dir)))
        (unless file-in-tags
          ;; TODO use relative or absolute path? For now, we'll use
          ;; absolute paths. How often do you move your source code OR
          ;; your TAGS file and not completely rebuild TAGS?
          (setq cmd (concat "etags -o " tags-file-name " -a " file))
          (cond
           ((eq etu/append-file-prompt 'prompt)
            (unless (y-or-n-p (concat "Add " file " to the TAGS file? "))
              (throw 'etu/update-for-file nil)))
           ((eq etu/append-file-prompt 'never)
            (throw 'etu/update-for-file nil))
           (t (message "Ok, not adding this file to TAGS"))))
        (message "Refreshing TAGS file for %s..." file)
        (start-process-shell-command proc-name "*etags-update*" cmd)
        (set-process-sentinel (get-process proc-name) 'etu/update-cb)))))

(define-minor-mode etags-update-mode
  "Minor mode to update the TAGS file when a file is saved.

Requires etags-update.pl to be in your PATH. Does not use
tags-table-list, only tags-file-name. It is helpful to set
tags-revert-without-query to `t' to avoid tedious prompting."
  nil
  :global t
  :lighter " etu"
  (if etags-update-mode
      (add-hook 'after-save-hook 'etu/update-for-file)
    (remove-hook 'after-save-hook 'etu/update-for-file)))

(provide 'etags-update)

;; etags-update.el ends here

;;;; etags-update.el
;;
;; Minor mode to update the TAGS file when a file is saved
;;

(defgroup etags-update nil
  "Minor mode to update the TAGS file when a file is saved"
  :group 'tools)

(defcustom etu/append-using-font-lock t
  "If non-nil, will only offer to add a buffer to TAGS if the
buffer has font-lock-defaults set. This is a weak indicator
that the buffer represents code, not plain text."
  :type 'boolean
  :group 'etags-update)

(defcustom etu/append-file-action 'prompt
  "What action should be taken when a file not already in TAGS is saved?
If `nil', do not add the file to TAGS.
If `add', add the file.
If `prompt', ask if this file should be added.
If set to a function, the function should return one of 'add, 'prompt, or 'nil."
  :type '(choice (const add)
                 (const prompt)
                 (const nil)
                 (function))
  :group 'etags-update)

(defvar etu/proc-buf "*etags-update*"
  "Buffer where etags-update.pl will write stdout")

(defvar etu/no-prompt-files (make-hash-table :test 'equal)
  "A collection of files not to be prompted for in file append situations")

(defun etu/append-prompt-file ()
  "Remove the curent-buffers's file from the no-prompt-files
 collection. Then, when the file is saved and
 `etu/append-file-action' is 'prompt, will prompt to add this
 file, even if you've answered \"no\" to the prompt before."
  (interactive)
  (remhash (buffer-file-name (current-buffer)) etu/no-prompt-files))

(defun etu/tags-file-dir ()
  "Return full directory of the TAGS file (or nil if no tags buffer exists)"
  (when tags-file-name
      (with-current-buffer (get-file-buffer tags-file-name)
        (expand-file-name default-directory))))

(defun etu/file-str-in-tags-buffer (buffer file-str)
  "Given a file-str which is a relative or absolute filename,
find a matching file in the given TAGS buffer. Return the
matching filename or nil."
  (with-current-buffer buffer
    (save-excursion ; useful only for debugging, can remove later
      (goto-char (point-min))
      (let ((match))
        (catch 'loop-exit
          (while (and (search-forward file-str nil t) (not match))
            (beginning-of-line)
            ;; Capture the whole filename on this line. The regex also
            ;; ensures we're on a 'file' line
            (if (re-search-forward "^\\(.*\\),[0-9]+$" nil t)
                (let ((file-in-tags (buffer-substring (match-beginning 1) (match-end 1))))
                  (when (string= file-str file-in-tags)
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
    (or (etu/file-str-in-tags-buffer tags-buffer file)
        (and (string= file (concat tags-dir file-rel)) ; ensure file-rel is in tags-dir
             (etu/file-str-in-tags-buffer tags-buffer file-rel)))))

(defun etu/test-file-str-in-tags-buffer ()
  "Testing utu/file-str-in-tags-buffer"
  (with-temp-buffer
    (insert "junkline\n")
    (insert "/home/mk/foo,10\n")
    (insert "bar,10\n")
    (insert "/home/mk/abcdefg,10\n")
    (assert (string= "/home/mk/foo" (etu/file-str-in-tags-buffer (current-buffer) "/home/mk/foo")))
    (assert (string= "bar" (etu/file-str-in-tags-buffer (current-buffer) "bar")))
    (assert (string= "/home/mk/abcdefg" (etu/file-str-in-tags-buffer (current-buffer) "/home/mk/abcdefg")))
    (assert (null (etu/file-str-in-tags-buffer (current-buffer) "/home/mk/abc")))
    (assert (null (etu/file-str-in-tags-buffer (current-buffer) "bcd")))
    (assert (null (etu/file-str-in-tags-buffer (current-buffer) "mk/abcdefg")))
    (assert (null (etu/file-str-in-tags-buffer (current-buffer) "junkline")))))

(defun etu/update-cb (process event)
  "Callback fn to handle etags-update.pl termination"
  (cond
   ((string= event "finished\n")
    ;; Manualy re-visit TAGS if we appended a new file -- we might not
    ;; have use find-tags between saves and we don't want to re-prompt
    ;; to add the file.
    (visit-tags-table (expand-file-name tags-file-name))
    (message "Refreshing TAGS file ...done")
    (when (get-buffer  etu/proc-buf)
      (kill-buffer (get-buffer etu/proc-buf))))
   (t (message "Refreshing TAGS file failed. Event was %s. See buffer %s." event etu/proc-buf))))

(defun etu/append-file-p (file)
  "Should we add this file to TAGS?"
  (let ((action etu/append-file-action))
    (when (functionp etu/append-file-action)
      (setq action (funcall etu/append-file-action file)))
    (cond
     ((eq action 'nil) nil)
     ((eq action 'add) t)
     ((eq action 'prompt)
      (cond
       ((gethash file etu/no-prompt-files) nil)
       ((and etu/append-using-font-lock (null font-lock-defaults)) nil)
       ((y-or-n-p (concat "Add " file " to the TAGS file? "))
        (progn
          (puthash file 1 etu/no-prompt-files)
          t))
       (t nil)))
     (t (error "Invalid etu/append-file-action action: %s" action)))))

(defun etu/update-tags-for-file ()
  "Update the TAGS file for the file of the current buffer. If
the file is not already in TAGS, maybe add it."
  (interactive)
  (catch 'etu/update-tags-for-file
    (when tags-file-name
      (let ((tags-file-full-name (expand-file-name tags-file-name)))
        (unless (get-file-buffer tags-file-full-name)
          (visit-tags-table tags-file-full-name))
        (assert (get-file-buffer tags-file-full-name)))
      (let* ((file              (buffer-file-name (current-buffer)))
             (file-in-tags      (etu/file-in-tags file))
             (cmd               (concat "etags-update.pl " tags-file-name " " file-in-tags))
             (proc-name         "etags-update")
             (default-directory (etu/tags-file-dir)))
        (if (string= file tags-file-name)
            (throw 'etu/update-tags-for-file nil))
        (unless file-in-tags
          (unless (etu/append-file-p file)
            (throw 'etu/update-tags-for-file nil))
          ;; TODO use relative or absolute path? For now, we'll use
          ;; absolute paths. How often do you move your source code OR
          ;; your TAGS file and not completely rebuild TAGS?
          (setq cmd (concat "etags -o " tags-file-name " -a " file)))
        (message "Refreshing TAGS file for %s..." file)
        (start-process-shell-command proc-name etu/proc-buf cmd)
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
      (add-hook 'after-save-hook 'etu/update-tags-for-file)
    (remove-hook 'after-save-hook 'etu/update-tags-for-file)))

(provide 'etags-update)

;; etags-update.el ends here

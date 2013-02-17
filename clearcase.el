;;; --------------------------------------------------------------------
;;; clearcase utils
;;; --------------------------------------------------------------------

(defun clt-lsvtree ()
  "Run clt lsvtree -g on the current buffer's file"
  (interactive)
  (shell-command (concat "clt lsvtree -g \"" (buffer-file-name) "\" &")))

(defun clt-diff ()
  "Run clt lsvtree -g on the current buffer's file"
  (interactive)
  (shell-command (concat "clt diff -g -predecessor \"" (buffer-file-name) "\" &")))

(defun clt-co ()
  "Checkout the current buffer's file"
  (interactive)
  (when (= 0 (shell-command (concat "clt co -nc \"" (buffer-file-name) "\"")))
    (revert-buffer)))

(defun clt-ci (comment)
  "Checkin the current buffer's file"
  (interactive "sComment: ")
  (let* ((c (if (and comment (> (length comment) 0)) comment nil))
         (cmd (concat "clt ci " 
                      (if c (concat "-c \"" c "\" ") "-nc ")
                      (buffer-file-name))))
    (when (= 0 (shell-command cmd))
    (revert-buffer t t))))

(defun clt-unco ()
  "Uncheckout the current buffer's file"
  (interactive)
  (when (= 0 (shell-command (concat "clt unco -keep \"" (buffer-file-name) "\"")))
    (revert-buffer)))

(defun clt-ls ()
  "See clearcase status of the current buffer's file"
  (interactive)
  (shell-command (concat "clt ls -long " (buffer-file-name) " &")))

(defun clt-annotate ()
  "Who wrote this horrible, horrible code?"
  (interactive)
  (shell-command (concat "clt annotate " (buffer-file-name) " "))
  (find-file (concat (buffer-file-name) ".ann")))

(defun clt-lsco ()
  "List checkouts on current branch. Uses current-directory."
  (interactive)
  (shell-command "clt lsco -cvi -avo -sh" "*checkouts*"))

(defun clt-made-on-this-branch ()
  "List files versions made in this branch."
  (interactive)
  ;; TODO: fragile: assumes branch name is 3rd line in config spec
  (shell-command "clt catcs | head -3 | tail -1 | sed 's/^# *//' | xargs -IBRANCH clt find -avobs -branch 'brtype(BRANCH)' -print -nxname" "*made-on-this-branch*"))

(defun clt-lspriv () 
  "List private files starting from current directory"
  (interactive)
  (shell-command "clt lsprivate . | egrep -v checkedout | egrep -v '#.*#' | egrep -v '*.class$'" "*private-files*"))

(defun clt-explore () 
  "Start clearexplorer in current directory"
  (interactive)
  (shell-command "clearexplorer . &" "*clt-explorer*"))

(defun clt-pred ()
  "List clearcase predecessor"
  (interactive)
  (let* ((cmd (concat "clt describe -fmt \"%En@@%PSn\n\" " (buffer-file-name)))
         (pred (shell-command-to-string cmd))
         (pred-len (length pred))
         (predd (if (> pred-len 1) (substring pred 0 (- pred-len 1)) pred)))
    (message "Predecessor is %s" predd)
    predd))

(defun clt-ediff () 
  "Ediff to Clearcase predecessor"
  (interactive)
  (let ((pred (clt-pred)))
    (message "Predecessor is %s" pred)
    (if (file-exists-p pred)
        (ediff-files pred (buffer-file-name)))))

(provide 'clearcase)

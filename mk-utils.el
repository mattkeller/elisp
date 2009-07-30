;;;; mk-utils.el - my elisp utility functions

(defun mk-iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun mk-tag-region (element)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive "sTag region with element: ")
  (save-excursion
    (when (and element (not (equal element "")))
      (when (string-match "[ \t]*$" element)
        (setq element (replace-match "" nil nil element))) ; trim whitespace
      (when (string-match "^[ \t]*" element)
        (setq element (replace-match "" nil nil element))) ; trim whitespace
      (goto-char (region-end))
      (insert "</" element ">")
      (goto-char (region-beginning))
      (insert "<" element ">"))))

(defun mk-vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun mk-vi-open-prev-line (arg)
  "Move to the prev line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (previous-line 1)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(global-set-key [(control o)] 'mk-vi-open-next-line)
(global-set-key [(control O)] 'mk-vi-open-prev-line)

(defun mk-occur-at-point()
  "invokes occur with the thing-at-point"
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'other-window)))

(global-set-key "\C-cg" 'mk-occur-at-point)

(defun mk-goto-repl ()
  "Open the slime REPL buffer in the current window"
  (interactive)
  (when (get-buffer "*slime-repl sbcl*")
    (switch-to-buffer "*slime-repl sbcl*")))

(defun mk-line-to-window-top ()
  (interactive)
  (recenter-top-bottom 1))

(global-set-key (kbd "C-c z") 'mk-line-to-window-top)

 (defun mk-shell-dwim (&optional create)
   "Start or switch to an inferior shell process, in a smart way.

 If a buffer with a running shell process exists, simply switch
 to that buffer. If a shell buffer exists, but the shell process
 is not running, restart the shell. If already in an active shell
 buffer, switch to the next one, if any. With prefix argument,
 CREATE a new shell."
   (interactive "P")
   (let* ((next-shell-buffer
           (catch 'found
             (dolist (buffer (reverse (buffer-list)))
               (when (string-match "^\\*shell\\*" (buffer-name buffer))
                 (throw 'found buffer)))))
          (buffer (if create
                      (generate-new-buffer-name "*shell*")
                    next-shell-buffer)))
     (shell buffer)))

(defun grep-kill ()
  "Alias for kill-grep"
  (interactive)
  (kill-grep))

;; from http://www.xsteve.at/prg/emacs/xsteve-functions.el
(defun mk-remove-control-M ()
  "Remove ^M at end of line in the whole buffer"
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

;; from http://www.xsteve.at/prg/emacs/xsteve-functions.el
(defun mk-smart-home()
  "Move to column 0, If repeated, move to first non-space"
  (interactive)
  (if (and (eq last-command 'mk-smart-home)
           (= (line-beginning-position) (point)))
    (beginning-of-line-text)
    (beginning-of-line)))

(global-set-key (kbd "C-a") 'mk-smart-home) ; was 'move-beginning-of-line

;; from http://www.emacswiki.org/emacs/WThreeMHintsAndTips
(defun mk-w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) filename)
          (w3m-find-file filename))
      (delete-file filename))))
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

(defun mk-arrow-keys-off ()
  (interactive)
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  (message "Arrow keys are off"))

(defun mk-arrow-keys-on ()
  (interactive)
  (global-set-key (kbd "<left>")  'backward-char)
  (global-set-key (kbd "<right>") 'forward-char)
  (global-set-key (kbd "<up>")    'previous-line)
  (global-set-key (kbd "<down>")  'next-line)
  (message "Arrow keys are on"))

(defun mk-toggle-window-split ()
  "Horizontal split to vertical and back again."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                  'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun mk-swap-windows (arg)
  "Swap the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun mk-ido-goto-symbol ()
    "Will update the imenu index and then use ido to select a symbol to navigate to"
    (interactive)
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (flet ((addsymbols (symbol-list)
               (when (listp symbol-list)
                 (dolist (symbol symbol-list)
                   (let ((name nil) (position nil))
                     (cond
                      ((and (listp symbol) (imenu--subalist-p symbol))
                       (addsymbols symbol))
                      ((listp symbol)
                       (setq name (car symbol))
                       (setq position (cdr symbol)))
                      ((stringp symbol)
                       (setq name symbol)
                       (setq position (get-text-property 1 'org-imenu-marker symbol))))
                     (unless (or (null position) (null name))
                       (add-to-list 'symbol-names name)
                       (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (cond
         ((overlayp position)
          (goto-char (overlay-start position)))
         (t
          (goto-char position))))))

(defun mk-align= ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "= "))

;; From http://www.emacswiki.org/emacs/TrampMode -----------------------

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
   	 ;; use a separate history list for "root" files.
   	 (file-name-history find-file-root-history)
   	 (name (or buffer-file-name default-directory))
   	 (tramp (and (tramp-tramp-file-p name)
   		     (tramp-dissect-file-name name)))
   	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
   	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(defun mk-agenda-to-html (html-file)
  "Write our org-agenda TODOs and 2 weeks of agenda to the given file"
  (interactive)
  (let* ((org-agenda-buffer-name "*mk-org-agenda*"))
    ;; open our agenda in a buffer with our private name
    (org-agenda-list t nil 14)
    (condition-case err
        (htmlize-buffer)
      (error (message "Failed to htmlize agenda: %s" (error-message-string err))))
    (when (get-buffer "*html*")
      (with-current-buffer "*html*"
        (write-file html-file)
        (kill-this-buffer)))
    (kill-buffer org-agenda-buffer-name)))


;; etags.el used to define this very useful function (according to
;; http://www.emacswiki.org/emacs/HippieExpand)

(defun tags-complete-tag (string predicate what)
     (save-excursion
     ;; If we need to ask for the tag table, allow that.
     (if (eq what t)
	(all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))

(defun mk-ecb-toggle ()
  "Toggle ECB windows on/foo. Activate ECB if needed."
  (interactive)
  (require 'ecb)
  (if ecb-minor-mode
      (ecb-toggle-ecb-windows)
    (ecb-activate)))

;; from http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun mk-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defun mk-pretty-print-xml-buffer ()
  (interactive)
  (mk-pretty-print-xml-region (point-min) (point-max)))

(defun mk-hide-java-imports ()
  "Hide the Java import statements in this file. Use
`hs-show-all' to show them again."
  (interactive)
  (hs-minor-mode 1)
  (let ((b -1)
        (e -1))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^import " nil t)
        (beginning-of-line)
        (setq b (point)))
      (goto-char (point-max))
      (when (re-search-backward "^import " nil t)
        (end-of-line)
        (setq e (point))))
    (when (and (not (= b -1))
               (not (= e -1)))
      (hs-make-overlay b e 'code))))

(defun mk-show-java-imports ()
  (interactive)
  (hs-show-all))

(defun mk-local-comment-auto-fill ()
  (interactive)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun mk-add-watchwords ()
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(autoload 'idle-highlight-mode "idle-highlight-mode" "Highlight current symbol" t)

(defun mk-coding-hook ()
  (interactive)
  ;(flyspell-prog-mode)
  ;(mk-local-comment-auto-fill)
  (mk-add-watchwords)
  (linum-mode 1)
  (idle-highlight-mode 1))

(defun mk-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; from https://github.com/nakkaya/emacs/blob/master/prog.el
(defun mk-bounce-sexp ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (error "%s" "Not on a paren, brace, or bracket")))))

(defun mk-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(provide 'mk-utils)

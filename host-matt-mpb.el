(require 'cider)

(set-face-attribute 'default nil :font "Bitstream Vera Sans Mono-14")

;; for lein for cider-jack-in
(add-to-list 'exec-path "/usr/local/bin")

;; 3-finger swipe produces annoying messages
;; TODO Put this in a OSX file
(dolist (multiple '("" "double-" "triple-"))
  (dolist (direction '("right" "left"))
    (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))

(defun zs-cider ()
  (interactive)
  (cider-connect "local.zsinternal.com" 44553))

(defun ido-switch-magit-ibuffer ()
  "Switch to a magit status buffer via `ido'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method
                       nil "magit status: " nil "*magit: "))

(defun mk-magit-for-project ()
  (interactive)
  (if mk-proj-name
      (magit-status mk-proj-basedir)
    (message "No project configured")))

(global-set-key (kbd "C-M-g") 'ido-switch-magit-buffer)

(setq mk-proj-ack-cmd "/usr/local/bin/ag")
(setq mk-proj-ack-default-args "--nocolor --nogroup")
(setq mk-proj-ack-respect-case-fold nil)

;; grep can't take -e ?
;; no ack cmd
;; ignore .m2 dir
;; Cider mode maps M-., overrides tags jump
;; etags regex's probably not finding certain legal clojure symbol names

(let ((pd "/Users/matt/.project/zensight/")
      (bd "/Users/matt/work/zensight"))
  (project-def "zensight"
               `((basedir          ,bd)
                 (src-patterns     ("*.clj" ".rb" ".py"))
                 (ignore-patterns  ("*.class" ".jar" "*.js"))
                 (vcs              git)
                 (tags-args        "--regex='/[ \\t\\(]*def[a-z]*-? \\([0-9a-z-!<>\?]+\\)/\\1/' --regex='/[ \\t\\(]*ns \\([a-z.]+\\)/\\1/'")
                 (grep-find-cmd    "find . -type f -not -path '*/.git/*' -not -path '*/.m2/*' -not -path '*/log/*' -not -name '*.jar' -not -path '*s3data*' -print0")
                 (index-find-cmd   "find . -type f -not -path '*/.git/*' -not -path '*/.m2/*' -not -path '*s3data*' -not -name '*.class' -not -name '*#'")
                 (tags-file        ,(concat pd "TAGS"))
                 (open-files-cache ,(concat pd "open-files"))
                 (file-list-cache  ,(concat pd "file-list-cache") nil))))

(let ((pd "/Users/matt/.project/zstack/")
      (bd "/Users/matt/work/zstack"))
  (project-def "zstack"
               `((basedir          ,bd)
                 (src-patterns     ("*.rb"))
                 (vcs              git)
                 (grep-find-cmd    "find . -type f -not -path '*/.git/*' -print0")
                 (index-find-cmd   "find . -type f -not -path '*/.git/*' -not -name '*#' -not -path '*/.sass-cache/*'")
                 (tags-file        ,(concat pd "TAGS"))
                 (open-files-cache ,(concat pd "open-files"))
                 (file-list-cache  ,(concat pd "file-list-cache") nil))))

(defun mk-open-file-system (file)
  (interactive "F")
  (let ((abs-file (expand-file-name file)))
    (message "Opening '%s'..." abs-file)
    (start-process-shell-command "open" "mk-open-file-system" (concat "open '" abs-file "'"))
    (message "Opening '%s'... done" abs-file)))

(defun mk-open-collateral ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "," nil t 1)
      (backward-char 1)
      (copy-region-as-kill (line-beginning-position) (point))
      (mk-open-file-system (car kill-ring-yank-pointer))))
  (end-of-line))


(global-set-key (kbd "C-c C-o") 'mk-open-collateral)

(global-set-key [F9] (lambda () (jump-to-register 49)))
(global-set-key [F10] (lambda () (jump-to-register 50)))

(setq visible-bell nil)

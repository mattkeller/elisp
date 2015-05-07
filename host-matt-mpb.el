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
  (cider-connect "192.168.59.103" 44553))

(defun ido-switch-magit-buffer ()
  "Switch to a magit status buffer via `ido'."
  (interactive)
  (ido-buffer-internal ido-default-buffer-method
                       nil "magit status: " nil "*magit: "))

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
      (bd "/Users/matt/zensight"))
  (project-def "zensight"
               `((basedir          ,bd)
                 (src-patterns     ("*.clj" ".rb" ".py"))
                 (ignore-patterns  ("*.class" ".jar"))
                 (vcs              git)
                 (tags-args        "--regex='/[ \\t\\(]*def[a-z]*-? \\([0-9a-z-!<>\?]+\\)/\\1/' --regex='/[ \\t\\(]*ns \\([a-z.]+\\)/\\1/'")
                 (grep-find-cmd    ,(concat "find . -type f -not -path '*/.git/*' -not -path '*/.m2/*' -not -path '*/log/*' -not -name '*.jar' -print0"))
                 (tags-file        ,(concat pd "TAGS"))
                 (open-files-cache ,(concat pd "open-files"))
                 (file-list-cache  ,(concat pd "file-list-cache") nil))))

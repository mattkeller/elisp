(require 'cider)

(defun zs-cider ()
  (interactive)
  (cider-connect "192.168.59.103" 44553))


;; grep can't take -e ?
;; no ack cmd
;; ignore .m2 dir
;; Cider mode maps M-., overrides tags jump
;; etags regex's probably not finding certain legal clojure symbol names

(let ((pd "/Users/matt/.project/zensight/")
      (bd "/Users/matt/Documents/zensight"))
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

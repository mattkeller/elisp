;;;; projects.el --- my mk-project settings

(require 'mk-project)
(require 'cl)

(setq mk-proj-use-ido-selection t)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file-ido)
(global-set-key (kbd "C-c p F") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(defvar homedir (concat (getenv "HOME") "/"))
(defvar projdir (concat homedir ".mk-project/"))

;;; --------------------------------------------------------------------
;;; elisp
;;; --------------------------------------------------------------------

(let ((pd (concat projdir "elisp/")))
  (project-def "elisp"
               `((basedir ,(concat homedir "elisp/"))
                 (src-patterns ("*.el"))
                 (file-list-cache ,(concat pd "files"))
                 (open-files-cache ,(concat pd "open-files"))
                 (ignore-patterns ("*.elc"))
                 (tags-file ,(concat pd "TAGS"))
                 (vcs git)
                 (startup-hook elisp-startup-hook))))

(defun elisp-startup-hook ()
  (find-file (concat mk-proj-basedir "init.el")))


(provide 'projects)


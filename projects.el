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


;;; --------------------------------------------------------------------
;;; mk-project / ibuffer integration
;;; --------------------------------------------------------------------

(require 'ibuffer-git) ; git-status-mini, git-status columns
(require 'ibuf-ext)

(defun mk/proj-buffer-p (b)
  "Is the buffer `b' part of the project?"
  (and mk-proj-name
       (or (mk-proj-buffer-p b)
           (string= (buffer-name b) mk-proj-fib-name)
           (string= (buffer-file-name b) mk-proj-tags-file))))

(define-ibuffer-column mk-proj-col
  (:name "P")
  (if (mk/proj-buffer-p buffer) "p" ""))

;; Define 3 formats, each including the new mk-proj-p-col
;; column. Switch formats with ibuffer-switch-format (bound to "`").
(setq ibuffer-formats
      '((mark modified read-only
              (mk-proj-col 2 2 :left :elide) " "
              (name 30 30 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)
        (mark modified read-only
              (mk-proj-col 2 2 :left :elide) " "
              (name 45 -1 :left) " "
              filename-and-process)
        (mark modified read-only
              (mk-proj-col 2 2 :left :elide) " "
              filename-and-process)
        (mark modified read-only
              mk-proj-col
              git-status-mini " "
              (name 30 30 :left :elide) " "
              (mode 16 16 :left :elide) " "
              (git-status 8 8 :left) " "
              filename-and-process)))

(define-key ibuffer-mode-map (kbd "C-c C-c") 'ibuffer-switch-format)

(define-ibuffer-filter project
  "Toggle current view to buffers in the defined mk-project."
  (:description "mk-project")
  (mk/proj-buffer-p buf))

(define-key ibuffer-mode-map (kbd "/ k") 'ibuffer-filter-by-project)

;; (define-ibuffer-column mk-proj-name-col
;;   (:name "Project")
;;   (if (mk-proj-proj-buffer-p buffer) mk-proj-name ""))

(provide 'projects)


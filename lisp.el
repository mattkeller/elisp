;;;; lisp.el 

;;; Clojure via inferior-lisp


(defun mk-inferior-clojure-12 ()
  (interactive)
  (mk-inferior-clojure (concat (getenv "HOME") "/.m2/repository/org/clojure/clojure/1.2.0/clojure-1.2.0.jar")))

(defun mk-inferior-clojure-13 ()
  (interactive)
  (mk-inferior-clojure (concat (getenv "HOME") "/.m2/repository/org/clojure/clojure/1.3.0-beta1/clojure-1.3.0-beta1.jar")))

(defun mk-inferior-clojure (clojure-jar)
  (interactive "fClojure jar file: ")
  (let* ((repl-cmd (concat "java -server -cp " clojure-jar " clojure.main")))
    (message repl-cmd)
    (inferior-lisp repl-cmd)))

(concat "java -server -cp " (concat (getenv "HOME") "/.m2/repository/org/clojure/clojure/1.2.0/clojure-1.2.0.jar") " clojure.main")

;(setq *inferior-lisp* "java -server -cp /home/mk/.m2/repository/org/clojure/clojure/1.2.0/clojure-1.2.0.jar clojure.main")
(setq *inferior-lisp* "inferior-lisp-clj")

;;; SBCL/Slime Setup --------------------------------------------------------

;(setq slime-base (expand-file-name "~/tmp/slime-2010-11-28"))
;; (setq slime-base (expand-file-name "~/.emacs.d/elpa/slime-20100404"))
;; (add-to-list 'load-path slime-base)
;; (add-to-list 'load-path (concat slime-base "contrib"))
;; (require 'slime-autoloads)

;; (add-to-list 'load-path "/home/mk/elisp/lib/swank-clojure")
;; (require 'swank-clojure-autoload)

;; (eval-after-load "slime"
;;  (slime-setup '(slime-repl)))

;; (when (string-equal (symbol-name system-type) "gnu/linux")
;;   (setq inferior-lisp-program "/opt/bin/sbcl --noinform")
;;   (autoload 'slime "slime" "Start and connect to the inferior lisp image" t)
;;   (autoload 'slime-mode "slime" "Start slime-mode for this buffer" t)
;;   (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
;;   (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

;;   (eval-after-load "slime"
;;     '(progn
;;        (add-to-list 'slime-lisp-implementations '(sbcl ("/opt/bin/sbcl" "--noinform")))

;;        (slime-setup '(slime-fancy slime-asdf slime-banner slime-highlight-edits))

;;        (setq slime-complete-symbol*-fancy t
;;              slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;              slime-when-complete-filename-expand t
;;              slime-truncate-lines nil
;;              slime-autodoc-use-multiline-p t
;;              slime-startup-animation nil)

;;        (define-key slime-mode-map      (kbd "C-TAB")   'slime-fuzzy-complete-symbol)
;;        (define-key slime-repl-mode-map (kbd "C-TAB")   'slime-fuzzy-complete-symbol)
;;        (define-key slime-mode-map      (kbd "TAB")     'slime-indent-and-complete-symbol)
;;        (define-key slime-mode-map      (kbd "C-c ;")   'slime-insert-balanced-comments)
;;        (define-key slime-repl-mode-map (kbd "C-c ;")   'slime-insert-balanced-comments)
;;        (define-key slime-mode-map      (kbd "C-c M-;") 'slime-remove-balanced-comments)
;;        (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)
;;        (define-key slime-mode-map      (kbd "RET")     'newline-and-indent)
;;        (define-key slime-mode-map      (kbd "")        'newline-and-indent)
;;        (define-key slime-mode-map      (kbd "C-j")     'newline)
;;        (define-key slime-mode-map      (kbd "<f5>")    'slime-selector)
;;        (define-key slime-repl-mode-map (kbd "<f5>")    'slime-selector)
;;        (define-key slime-mode-map      (kbd "C-c r")   'mk-goto-repl)

;;       (paredit-mode +1))))

; do slime mode for all lisp files
(add-hook 'lisp-mode-hook (lambda ()
                            ;; (cond ((not (featurep 'slime))
                            ;;        (require 'slime)
                            ;;      (normal-mode)))
			    (modify-syntax-entry ?- "w")))

;;; Clojure ------------------------------------------------------------

;;(add-to-list 'load-path (concat dotfiles-dir "lib/clojure-mode"))
;;(require 'clojure-mode)
;(require 'swank-clojure) ; start swank from lein or whatever, not emacs

;; (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;; (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;; (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;; (eval-after-load "slime"
;;   '(progn
;;      (require 'swank-clojure)
;;      (add-to-list 'slime-lisp-implementations `(clojure ,(swank-clojure-cmd) :init swank-clojure-init) t)
;;      (add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
;;      (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
;;      (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)))

;;(setq mk-clojure-jar "/opt/clojure/lib/clojure.jar")
;; (setq mk-clojure-contrib-jar "/opt/clojure/lib/clojure-contrib.jar")

;; (setq swank-clojure-jar-path mk-clojure-jar)
;; (add-to-list 'swank-clojure-extra-classpaths mk-clojure-contrib-jar)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; Sbcl is still the default slime target. Launch clojure with M-- M-x slime clojure

(defun clojure-project (path clojure-jar clojure-contrib-jar class-dir classpath-dirs)
  "Sets up classpath for a clojure project and starts a new SLIME session."
  (interactive)
  (when (get-buffer "*inferior-lisp*")
    (kill-buffer "*inferior-lisp*"))
  (setq swank-clojure-binary nil
        swank-clojure-jar-path (or clojure-jar mk-clojure-jar)
        swank-clojure-extra-classpaths
        (cons (or clojure-contrib-jar mk-clojure-contrib-jar)
              classpath-dirs)
        swank-clojure-extra-vm-args
        (list "-server"
              "-Xdebug"
              "-Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n"
              (format "-Dclojure.compile.path=%s" (expand-file-name class-dir path)))
        ;; moves clojure to the front (default) position in the list
        slime-lisp-implementations
        (cons `(clojure ,(swank-clojure-cmd) :init swank-clojure-init)
              (remove-if '(lambda (x) (eq (car x) 'clojure))
                         slime-lisp-implementations)))
  (save-window-excursion
    (slime)))

(defun mk-lein-repl ()
  "Launch a clojure REPL with lein in the project's root"
  (interactive)
  (if (and mk-proj-basedir
           (file-exists-p (concat mk-proj-basedir "project.clj")))
      (let ((default-directory mk-proj-basedir))
        (inferior-lisp "lein repl"))
    (message "Sorry, no project.clj at the project basedir")))

(defun mk-lein-swank ()
  "Launch a swank server with lein in the project's root"
  (interactive)
  (if (and mk-proj-basedir
           (file-exists-p (concat mk-proj-basedir "project.clj")))
      (let ((default-directory mk-proj-basedir))
        (inferior-lisp "lein swank"))
    (message "Sorry, no project.clj at the project basedir")))

(defun mk-lein-new-project (name dir)
  "Create a new clojure project with 'lein new'"
  (interactive "sProject Name: \nDDirectory: ")
  (let ((default-directory dir))
    (shell-command (concat "lein new " name " ."))))

(defun mk-lein-test ()
  "Run 'lein test' in the project's basedir"
  (interactive)
  (when (and mk-proj-basedir 
             (file-exists-p (concat mk-proj-basedir "project.clj")))
    (let ((default-directory mk-proj-basedir))
      (shell-command "lein test"))))

(defun mk-clojure-mk-project (name dir)
  "Create a new clojure project in `dir' with lein and add a mk-project for it."
  (interactive "sProject Name: \nFDirectory: ")
  (let ((pd (file-name-as-directory (concat (getenv "HOME") "/.mk-project/" name)))
        (bd (file-name-as-directory dir)))
    (unless (file-exists-p pd) (mkdir pd))
    (unless (file-exists-p bd) (mkdir bd))
    (message (concat "PD: " pd "; BD " bd))
    (unless (file-exists-p (concat bd "/project.clj"))
      (mk-lein-new-project name bd)
      (message "Created lein project"))
    (project-def name `((basedir ,bd)
                        (src-patterns ("*.clj"))
                        (ignore-patterns ("*.class"))
                        (compile-cmd "lein compile")
                        (tags-file ,(concat pd "/TAGS"))
                        (file-list-cache ,(concat pd "/file-index"))
                        (open-files-cache ,(concat pd "/open-files"))
                        (startup-hook ,(intern-soft (concat "mk-" name "-project-startup")))
                        (vcs git)))
    (message (concat "Created project " name " in " dir))))


;;; Elisp Setup --------------------------------------------------------

(defun mk-remove-elc-on-save ()
  "If saving an elisp file, remove the .elc file"
  (make-local-variable 'after-save-ook)
  (add-hook 'after-save-hook
            (lambda ()
              (let ((elc-file (concat buffer-file-name "c")))
                (when (file-exists-p elc-file)
                    (message "Removing %s" elc-file)
                    (delete-file elc-file))))))

(add-hook 'emacs-lisp-mode-hook 
          (lambda ()
            (turn-on-eldoc-mode)
            (mk-remove-elc-on-save)))

(provide 'lisp)

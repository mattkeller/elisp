;;;; java.el -- java coding config

(add-to-list 'load-path "~/elisp/elib-1.0")
(add-to-list 'load-path "~/elisp/cedet-1.0pre6/common/")
(add-to-list 'load-path "~/elisp/jdee-2.4.0/lisp")

(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory   "~/.semantic.cache"
      semanticdb-persistent-path          nil)

;; Load JDE (and prereqs, like cedet) when we first load a java file,
;; not on startup
(autoload 'jde-mode "jde" "JDE mode." t)
(add-to-list 'auto-mode-alist '("\\.java$" . jde-mode))

(when (getenv "JAVA_HOME")
  (let* ((path (getenv "JAVA_HOME"))
         (version (file-name-nondirectory path)))
    (setq jde-jdk-registry `((,version . ,(getenv "JAVA_HOME")))
          jde-jdk           version)))

(add-hook 'jde-mode-hook '(lambda () 
                            (require 'jde-ant)
                            (message "jde-mode time")
                            (local-set-key [f5] 'jde-compile)))


(add-hook 'java-mode-hook '(lambda ()
                             (c-set-style "java")
                             (setq c-basic-offset 3)
                             (setq-default indent-tabs-mode nil
                                           tab-width 4)
                             (linum-mode)))

(provide 'java)

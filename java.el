;;;; java.el -- java coding config

(add-to-list 'load-path (concat dotfiles-dir "lib/elib-1.0"))
(add-to-list 'load-path (concat dotfiles-dir "lib/cedet-1.0pre6/common/"))

(setq semantic-load-turn-useful-things-on t
      semanticdb-default-save-directory   "~/.semantic.cache"
      semanticdb-persistent-path          nil)

(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))

(when (getenv "JAVA_HOME")
  (let* ((path (getenv "JAVA_HOME"))
         (version (file-name-nondirectory path)))
    (setq jde-jdk-registry `((,version . ,(getenv "JAVA_HOME")))
          jde-jdk           version)))

(add-hook 'java-mode-hook '(lambda ()
                             (coding-hook)
                             (c-set-style "java")
                             (setq c-basic-offset 3)
                             (setq-default indent-tabs-mode nil
                                           tab-width 4)
                             (subword-mode)
                             (mk-maybe-hide-imports)
                             (local-set-key (kbd "C-.") 'javadoc-lookup)))

(defun mk-maybe-hide-imports ()
  (save-excursion
    (goto-char (point-min))
    (when (>= (count-matches "^import ") 5)
    (message "Lotsa import statements, let's hide them")
    (mk-hide-java-imports))))

(provide 'java)

(use-package clojure-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'mk-coding-hook)
  (add-hook 'clojure-mode-hook #'rainbow-paren-mode)

  ;; todo rainbow-parent-mode

  ;; via ahinz
  (defun fix-clojure-indent ()
    (interactive)
    (put-clojure-indent 'nextTuple 'defun)
    (put-clojure-indent 'table 'defun)
    (put-clojure-indent 'tr 'defun)
    (put-clojure-indent 'td 'defun)
    (put-clojure-indent 'div 'defun)
    (put-clojure-indent 'header 'defun)
    (put-clojure-indent 'button 'defun)
    (put-clojure-indent 'span 'defun)
    (put-clojure-indent 'nav 'defun)
    (put-clojure-indent 'ul 'defun)
    (put-clojure-indent 'li 'defun)
    (put-clojure-indent 'GET 'defun)
    (put-clojure-indent 'POST 'defun)
    (put-clojure-indent 'fact 'defun)
    (put-clojure-indent 'routes 'defun)
    (put-clojure-indent 'emit-bolt! 'defun)
    (put-clojure-indent 'execute 'defun)))

(use-package cider
  :config
  (require 'cider-macroexpansion)) ; C-C RET

(use-package slamhound
  :defer t
  :ensure t
  :commands slamhound)

(provide 'clojure)

(use-package clojure-mode
  :ensure t
  :init
  (use-package rainbow-delimiters
    :ensure t)

  :config
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'mk-coding-hook)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook
            (lambda()
              (add-hook 'write-contents-functions #'mk-whitespace-cleanup))))

(use-package cider
  :init
  (use-package ac-cider
    :ensure t
    :bind (("C-c C-z" . cider-switch-to-repl-buffer)) ; global!
    :config
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    (eval-after-load "auto-complete"
      '(progn
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode))))

  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq nrepl-log-messages t) ; log to *nrepl-messages* buffer

  (require 'cider-macroexpansion) ; C-C RET
  (require 'cider-grimoire) ; C-c C-d C-g => cider-grimoire-web
  (require 'cider-classpath)

  (defun mk-cider-require (ns alias)
    (interactive "sNamespace: \nsAlias: ")
    (cider-interactive-eval
     (concat "(require '[" ns " :as " alias "])")))

  (global-set-key (kbd "C-c R") 'mk-cider-require))

(use-package slamhound
  :defer t
  :ensure t
  :commands slamhound)

;; via ahinz, fix indentation
;; (put-clojure-indent 'emit-bolt! 'defun)

(provide 'clojure)

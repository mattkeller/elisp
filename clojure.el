(add-to-list 'load-path (expand-file-name (concat dotfiles-dir "lib/clojure-mode")))

(autoload 'clojure-mode "clojure-mode" "Mode for editing clojure source files" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(require 'clojure-mode)
(require 'nrepl)
 
;; Configure nrepl.el
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.nrepl-history")
 
;; Some default eldoc facilities
(add-hook 'nrepl-connected-hook
          (defun mk-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))
 
;; Repl mode hook
(add-hook 'nrepl-mode-hook 'subword-mode)
 
(provide 'clojure)

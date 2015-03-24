;; cider require specific versions of clojure-mode
(add-to-list 'load-path (expand-file-name (concat dotfiles-dir "lib/clojure-mode")))
(add-to-list 'load-path (expand-file-name (concat dotfiles-dir "lib/cider-0.8.2")))

(autoload 'clojure-mode "clojure-mode" "Mode for editing clojure source files" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(require 'clojure-mode)
(require 'cider)

(add-hook 'clojure-mode-hook #'mk-coding-hook)
 
(provide 'clojure)


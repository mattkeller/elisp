(add-to-list 'load-path "~/clojure/slime-20100404")
(add-to-list 'load-path "~/clojure/slime-20100404/contrib")
(add-to-list 'load-path "~/clojure/clojure-mode")

(setq swank-clojure-jar-path "~/clojure/clojure-1.2.1.jar")

(require 'clojure-mode)

;; broken - can't find def of setup=slime-contribs
;; (require 'slime-autoloads) 
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
;(slime-setup)

(provide 'clojure)
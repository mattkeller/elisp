;;;; system-gnu/linux.el -- linux-only config

;; w3m config ----------------------------------------------------------

(add-to-list 'load-path (concat dotfiles-dir "lib/emacs-w3m"))
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq w3m-use-cookies t)

(defun mk-choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use firefox? ")
      (browse-url-firefox url)
    (w3m-browse-url url)))

(setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
				    ("javase"    . w3m-browse-url)
				    ("."         . mk-choose-browser)))


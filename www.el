;;;; www.el -- Web Browsing

(setq mk-browse-key "\C-cb")
(add-to-list 'load-path "~/elisp/emacs-w3m")
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key mk-browse-key 'browse-url-at-point)
(setq w3m-use-cookies t)

;; firefox.exe must be in the PATH
(if (string-equal system-type "windows-nt")
    (setq browse-url-firefox-program (executable-find "firefox.exe")))

(defun mk-choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use firefox? ")
      (browse-url-firefox url)
    (w3m-browse-url url)))

(setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
				    ("javase"    . w3m-browse-url)
				    ("."         . mk-choose-browser)))

(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map mk-browse-key 'mk-dired-browse-file)))

(defun mk-dired-browse-file ()
  "In dired, open html file at point in a web-browse"
  (interactive)
  (let ((file (dired-get-filename)))
    (when file (browse-url (concat "file://" file)))))

(defun mk-browse-current-buffer ()
  (interactive)
  (browse-url (concat "file://" (buffer-file-name (current-buffer)))))

;; TODO: write a dwim fn such that if I am at a URL, it opens the url;
;; if I am in a html buffer, run mk-browse-current-buffer.

(provide 'www)

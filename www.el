;;;; www.el -- Web Browsing

(setq mk-browse-key "\C-cb")
(global-set-key mk-browse-key 'browse-url-at-point)

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

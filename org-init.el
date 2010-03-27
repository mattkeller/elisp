;;;; org-mode.el setup

(add-to-list 'load-path (concat dotfiles-dir "lib/org-mode/lisp"))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(org-remember-insinuate)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)

(setq org-directory                     (expand-file-name "~/proj/org")
      org-default-notes-file            (concat org-directory "/todo.org")
      org-agenda-files                  (file-expand-wildcards (concat org-directory "/*.org"))
      org-startup-folded                nil
      org-log-done                      'time
      org-return-follows-link           t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done  t)

(defvar mk-org-save-timer
  ; git-auto-commit runs at top of hour
  (run-at-time "00:59" 3600 'org-save-all-org-buffers))

(setq org-remember-templates
      `(("Todo"    ?t "* TODO %?\n  %i\n" ,(concat org-directory "/todo.org")    "Inbox")
        ("Maybe"   ?m "* %?\n  %i\n"      ,(concat org-directory "/maybe.org")   "Misc")
        ("Journal" ?j "* %T %?\n\n  %i\n" ,(concat org-directory "/journal.org") bottom)
        ("Idea"    ?i "* %?\n  %i\n"      ,(concat org-directory "/maybe.org")   "Ideas")))

(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

(setq org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "VERIFY(v)" 
                       "|" "DONE(d)" "DEFERRED(f)" "CANCELED(c)")))

(when nil ; TODO mk-agenda-to-html is f*cking up the rest of org somehow
  (defvar mk-agenda-timer
    (if (string-equal system-name "mktop")
        (run-at-time "00:58" 300 'mk-agenda-to-html "~/.agenda.html")
      nil)))

(provide 'org-init)

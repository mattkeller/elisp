;;;; org-mode.el

;; Use stock version of org. Its is reasonably up-to-date in emacs 24.4.
(require 'org)
(require 'org-loaddefs)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)

(setq base-org                          (expand-file-name "~/gtd")
      home-org                          (expand-file-name (concat base-org "/home/"))
      work-org                          (expand-file-name (concat base-org "/work/"))
      org-directory                     work-org
      org-default-notes-file            (concat org-directory "/todo.org")
      org-agenda-files                  (list home-org work-org)
      org-startup-folded                nil
      org-log-done                      'time
      org-return-follows-link           t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done  t)

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-directory "/todo.org") "Inbox")
             "* TODO %?\n %i\n")
        ("m" "Maybe" entry (file+headline ,(concat org-directory "/maybe.org") "Misc")
              "* %?\n %i\n")
        ("i" "Idea" entry (file+headline ,(concat org-directory "/maybe.org") "Ideas")
             "* %?\n %i\n")))

(when (not noninteractive)
  (add-hook 'org-mode-hook (lambda ()
                             (flyspell-mode 1)
                             (org-indent-mode 1))))

(setq org-todo-keywords
           '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "VERIFY(v)"
                       "|" "DONE(d)" "DEFERRED(f)" "CANCELED(c)")))

(provide 'org-init)

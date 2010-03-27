;;;; lang.el -- misc lang setup

(dolist (mode '(c-mode java-mode cperl-mode emacs-lisp-mode ruby-mode
                       caml-mode lisp-mode clojure-mode))
  (font-lock-add-keywords mode '(("\\(XXX\\|FIXME\\|TODO\\)"
                                  1 font-lock-warning-face prepend))))

;;; Perl setup ---------------------------------------------------------

(when (maybe-load "cperl-mode")
  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  (add-hook 'cperl-mode-hook
            '(lambda ()
               (local-set-nkey [f5] 'perl-compile)
               (linum-mode))))

(defun perl-compile ()
  "Run perl -c against the current file"
  (interactive)
  (shell-command (concat "perl -c " (buffer-file-name))))

;;; Ruby setup ---------------------------------------------------------

(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook 'turn-on-font-lock)
(autoload 'rubydb "rubydb2x" "Ruby debugger" t)

(defun ruby-lint ()
  "Performs a Ruby compile check on the current file."
  (interactive)
  (shell-command (concat "ruby -c " (buffer-file-name))))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)
             (local-set-key [f5] 'ruby-lint)
             (linum-mode)))

;;; Javascript ---------------------------------------------------------

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

(require 'flymake-jslint)
(add-hook 'javascript-mode-hook (lambda () (flymake-mode t)))

;;; OCaml --------------------------------------------------------------

(add-to-list 'load-path (concat "lib/ocaml"))
(add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" .  caml-mode))
(autoload 'caml-mode "ocaml" "ocaml" "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" "camldebug" "Debug caml mode")

(provide 'lang)

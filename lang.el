;;;; lang.el -- misc lang setup

(dolist (mode '(c-mode java-mode cperl-mode emacs-lisp-mode ruby-mode
                       caml-mode lisp-mode clojure-mode))
  (font-lock-add-keywords mode '(("\\(XXX\\|FIXME\\|TODO\\)"
                                  1 font-lock-warning-face prepend))))

;;; C, C++ setup ------------------------------------------------------

(add-hook 'c-mode-common-hook '(lambda () (mk-coding-hook)))

(defun mk-c++-mode-hook ()
  (c-set-style "bsd")
  (setq-default c-basic-offset 4
                indent-tabs-mode nil
                tab-width 4)
  (c-toggle-auto-hungry-state 0))

(add-hook 'c++-mode-hook 'mk-c++-mode-hook)

;;; Perl setup ---------------------------------------------------------

(when (maybe-load "cperl-mode")
  (add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  (add-hook 'cperl-mode-hook
            '(lambda ()
               (local-set-nkey [f5] 'perl-compile)
               (mk-coding-hook))))

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
             (mk-coding-hook)))

;;; Javascript ---------------------------------------------------------

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

(require 'flymake-jslint)
(add-hook 'javascript-mode-hook (lambda ()
                                  (mk-coding-hook)
                                  (flymake-mode t)))

;;; OCaml --------------------------------------------------------------

(add-to-list 'load-path (concat "lib/ocaml"))
(add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" .  caml-mode))
(autoload 'caml-mode "ocaml" "ocaml" "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" "camldebug" "Debug caml mode")

;;; Protocol Buffers ---------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.proto$" .  protobuf-mode))
(autoload 'protobuf-mode "protobuf-mode" "protobuf-mode" t)

;;; Haskell ------------------------------------------------------------

(add-to-list 'load-path (concat dotfiles-dir "lib/haskell-mode-2.8.0"))
(autoload 'haskell-mode "haskell-site-file" "haskell-site-file" t)
(add-to-list 'auto-mode-alist '("\\.hs$" .  haskell-mode))

(provide 'lang)

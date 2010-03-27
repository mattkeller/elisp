;;;; system-bsd.el -- bsd systems

;;; server -------------------------------------------------------------

(when (< emacs-major-version 23) ; Using emacs23 --daemon now
  (start-named-server "server")) ; default server-name

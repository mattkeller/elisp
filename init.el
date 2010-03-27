;;;; init.el -- load me first! Assumes thirdparty libs are available in ./lib

(defvar emacs-start-time (float-time))

(require 'cl)

(defun maybe-load (file)
  "If we can find file on the load-path, load it and return t, otherwise nil"
  (interactive "sFile: ")
  (load file t t nil))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(message "Loading from %s" dotfiles-dir)

(add-to-list 'load-path dotfiles-dir)
(require 'basic)

(add-to-list 'load-path (concat dotfiles-dir "lib"))
(require 'mk-utils)
(require 'org-init)
(require 'java)
(require 'lisp)
(require 'lang)
(require 'www)

(maybe-load (concat "system-" (downcase (symbol-name system-type))))
(maybe-load (concat "host-" (downcase system-name)))
;; (when (string-equal system-name "kelma12*") (maybe-load "work"))

;;; Various libs and autoloads -----------------------------------------

(autoload 'find-file-recursively "find-recursive" "" t)
(autoload 'light-symbol-mode "light-symbol" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'typing-of-emacs "typing" "The Typing-Of-Emacs, a game" t)
(autoload 'magit-status "magit" "Magit git helper" t)

(add-to-list 'load-path (concat dotfiles-dir "lib/egg"))
(autoload 'egg-minor-mode "egg" "" t)

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(autoload 'ack "ack" "Ack is better than grep" t)
(autoload 'mcp-hl-mode "mcp-hl" "" t)
(autoload 'list-register "list-register" "" t)

(when (< emacs-major-version 23) 
  (autoload 'linum-mode "linum" "" t))

(require 'recentf-ext)
(setq recentf-max-menu-items 25)

;;; Emacs server -------------------------------------------------------

(require 'server)

(defun start-named-server (name)
  "Start a server named 'name' - ensure only 1 server of that name is running"
  (interactive "sServer Name: ")
  (setq server-name name)
  (setq mk-server-socket-file (concat server-socket-dir "/" name))
  (unless (server-running-p name)
    (server-start)))

(when (or (< emacs-major-version 23)               ; Using emacs23 --daemon now
          (string-equal system-type "windows-nt"))
  (start-named-server "server")) ; default server-name

;;; Emacs Code Browser -------------------------------------------------

(add-to-list 'load-path  (concat dotfiles-dir "lib/ecb-2.40"))
(require 'ecb-autoloads)
(setq ecb-layout-name "left10")
(add-hook 'ecb-before-activate-hook '(lambda () (require 'cedet)))

;;; NXML Setup ---------------------------------------------------------

(add-to-list 'load-path (concat dotfiles-dir "lib/nxml-mode"))
(autoload 'nxml-mode "nxml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode))

;;; etags-select -------------------------------------------------------

(autoload 'etags-select-find-tag "etags-select" "for etags-select method" t)
(setq etags-select-no-select-for-one-match t)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key [f7] 'etags-select-find-tag)

;;; Tramp (remote editing) ---------------------------------------------

(setq tramp-default-method "scp")
(setq tramp-default-user user-login-name)
(setq tramp-default-user-alist
               '(("scp" ".*\\.littleredbat\\.net\\" "mk")
                 ("scp" "lrb" "mk")))

(setq tramp-auto-save-directory "~/.tramp-autosave")

;;; Git VC backend setup -----------------------------------------------

(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

;;; Browse Kill Ring ---------------------------------------------------

(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)

;;; twitter.el ---------------------------------------------------------

(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;;; color-theme -------------------------------------------------------

(when window-system
  (add-to-list 'load-path (concat dotfiles-dir "lib/color-theme"))
  (require 'color-theme)
  (color-theme-initialize)

  (defun my-color-theme () 
    (interactive)
    (ecase (intern (completing-read "Theme: " '("light" "dark" "gray" "reset")))
      (light (color-theme-emacs-21))
      (dark  (color-theme-dark-laptop))
      (gray  (color-theme-jedit-grey))
      (reset (color-theme-snapshot))))

  (color-theme-dark-laptop))

;;; mk-projects --------------------------------------------------------

;(add-to-list 'load-path "~/code/mk-project")
(load "projects.el")

(dolist (mode '(c-mode java-mode cperl-mode emacs-lisp-mode ruby-mode
                       caml-mode lisp-mode clojure-mode))
  (font-lock-add-keywords mode '(("\\(XXX\\|FIXME\\|TODO\\)"
                                  1 font-lock-warning-face prepend))))

;;;; Utils --------------------------------------------------------------

;; using mk-utils
(mk-arrow-keys-off)
(define-key ctl-x-4-map "t" 'mk-toggle-window-split)
(define-key ctl-x-4-map "s" 'mk-swap-windows)
(defalias 'imenu 'mk-ido-goto-symbol "imenu using ido")
(global-set-key (kbd "C-x C-r") 'find-file-root)

(autoload 'etags-update-mode "etags-update" "sweet!" t)

(eval-after-load "etags-update.el"
  '(progn
     (defun mk-etags-update-append-file-p (file)
       (cond
        ((and mk-proj-name
              mk-proj-tags-file
              (string= mk-proj-basedir (substring file 0 (length mk-proj-basedir)))) ; eg, file *in* project
         ;; TODO (match mk-proj-src-patterns)
         ;; TODO (not-match mk-proj-ignore-patterns))
         'add)
        (t 'prompt)))

     (setq etu/append-file-prompt 'mk-etags-update-append-file-p)))

;;;; Uniqify ------------------------------------------------------------

(require 'uniquify) 
(setq uniquify-buffer-name-style 'post-forward ;; unique buffer names using 
      uniquify-separator         ":")          ;; part of file's path

;;;; smex (keep at bottom of .emacs) ------------------------------------

(require 'smex)
(setq smex-save-file "~/.smex.save"
      ido-enable-flex-matching t) ;; see also ido-enable-regexp
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)     ;; This is your old M-x.
(global-set-key (kbd "C-c C-c C-x C-m") 'execute-extended-command) ;; This is your old M-x.
;; (global-set-key (kbd "C-c M-x") 'smex-update-and-run)

(message "Emacs took %s seconds to start" (- (float-time) emacs-start-time))
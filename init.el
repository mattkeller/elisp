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

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path (concat dotfiles-dir "lib"))
(require 'mk-utils)
(require 'org-init)
(require 'java)
(require 'lang)
(require 'clojure)
(require 'www)
(require 'projects)
(require 'mk-tags)

;;; Various libs and autoloads -----------------------------------------

(autoload 'find-file-recursively "find-recursive" "" t)
(autoload 'light-symbol-mode "light-symbol" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'typing-of-emacs "typing" "The Typing-Of-Emacs, a game" t)

(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

(autoload 'ack "ack" "Ack is better than grep" t)
(autoload 'mcp-hl-mode "mcp-hl" "" t)
(autoload 'list-register "list-register" "" t)
(autoload 'rainbow-paren-mode "rainbow-parens" "" t)
(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

(when (< emacs-major-version 23) 
  (autoload 'linum-mode "linum" "" t))

;;; Recent files -------------------------------------------------------

(require 'recentf)
(recentf-mode 1)
(require 'recentf-ext)
(setq recentf-max-menu-items 25)

;;; Emacs server -------------------------------------------------------

;; no servers *started* from here, leave that to the host- files
(require 'server)

(defun is-server-running (name)
  "Check is an emacs-server process is already running"
  (interactive)
  (let ((socket-path (concat server-socket-dir "/" name)))
    ;; server-running-p defined only in emacs23
    (if (functionp 'server-running-p)
        (server-running-p socket-path)
      ;; fall back, not as reliable
      (file-exists-p socket-path))))

(defun start-named-server (name)
  "Start a server named 'name' - ensure only 1 server of that name is running"
  (interactive "sServer Name: ")
  (setq server-name name)
  (setq mk-server-socket-file (concat server-socket-dir "/" name))
  (unless (is-server-running name)
    (server-start)))

;;; Emacs Code Browser -------------------------------------------------

(add-to-list 'load-path  (concat dotfiles-dir "lib/ecb-2.40"))
(require 'ecb-autoloads)
(setq ecb-layout-name "left10")
(add-hook 'ecb-before-activate-hook '(lambda () (require 'cedet)))

;;; NXML Setup ---------------------------------------------------------

(autoload 'nxml-mode "nxml-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|pom\\)\\'" . nxml-mode))

;;; etags-select -------------------------------------------------------

(autoload 'etags-select-find-tag "etags-select" "for etags-select method" t)
(setq etags-select-no-select-for-one-match t)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key [f7] 'etags-select-find-tag)

;;; Tramp (remote editing) ---------------------------------------------

(setq tramp-default-method "scp")
(setq tramp-default-user user-login-name)
(setq tramp-auto-save-directory "~/.tramp-autosave")

;;; Git VC backend setup -----------------------------------------------

(if nil
    (progn
      (require 'vc-git)
      (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
      (require 'git)
      (autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t))
  (setq vc-handled-backends nil))

;;; Browse Kill Ring ---------------------------------------------------

(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :config
  (defadvice yank-pop (around kill-ring-browse-maybe (arg))
    "If last action was not a yank, run `browse-kill-ring' instead."
    (if (not (eq last-command 'yank))
        (browse-kill-ring)
      ad-do-it))
  (ad-activate 'yank-pop))

;;; color-theme -------------------------------------------------------

(defun is-daemonized () 
  (and (functionp 'daemonp) 
       (daemonp)))

(when (or window-system (is-daemonized))
  (use-package base16-theme
    :ensure t)
  (load-theme 'base16-default-dark t))

;;;; Utils --------------------------------------------------------------

;; using mk-utils
(mk-arrow-keys-on)
(defalias 'imenu 'mk-ido-goto-symbol "imenu using ido")
(global-set-key (kbd "C-c TAB") 'imenu)
(define-key ctl-x-4-map "t" 'mk-toggle-window-split)
(define-key ctl-x-4-map "s" 'mk-swap-windows)
(global-set-key (kbd "C-x C-r") 'find-file-root)
(global-set-key (kbd "C-c C-f") 'mk-recentf-ido-find-file)

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

;;; Host and System specific config -------------------------------------

(let ((hostname (downcase system-name))
      (systype  (downcase (symbol-name system-type))))

  (cond ((string= "windows-nt" systype)
         (maybe-load (concat "system-windows")))
        ((string= "gnu/linux" systype)
         (maybe-load (concat "system-linux")))
        ((string= "berkeley-unix" systype)
         (maybe-load (concat "system-bsd"))))

  (maybe-load (concat "host-" hostname))

  (when (and (>= (length hostname) (length "kelma12"))
             (string= "kelma12" (substring hostname 0 (length "kelma12"))))
    (maybe-load "work")))

(maybe-load "~/.emacs-local.el")


;;;; expand-region -----------------------------------------------------
(use-package expand-region
  :ensure t
  :bind (("C-c e" . er/expand-region)))

(use-package magit
  :ensure t
  :commands magit-status magit-blame-mode
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

;;;; smex (keep at bottom of .emacs) ------------------------------------

(require 'smex)
(setq smex-save-file "~/.smex.save"
 ido-enable-flex-matching t) ;; see also ido-enable-regexp
(smex-initialize)

(message "Emacs took %s seconds to start" (- (float-time) emacs-start-time))


(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(iswitchb-mode 1)                   ;; more informative buffer switching
(line-number-mode 1)
(column-number-mode 1)              ;; lines and columns, duh
(menu-bar-mode 1)                   ;; keyboard only, please
(tool-bar-mode 0)
(setq-default indent-tabs-mode nil) ;; by default, insert spaces, not a full tab
(setq visible-bell t)               ;; no beeping!
(fset 'yes-or-no-p 'y-or-n-p)       ;; query with y or n always
(setq inhibit-splash-screen t)
(blink-cursor-mode nil)             ;; no blinking!
(auto-compression-mode 1)           ;; inline edit files in gzip, bzip2 archives
(transient-mark-mode)

(setq iswitchb-buffer-ignore '("^ " "*Buffer" "*Messages*" "*Help*"))
(set-background-color "gray97")

(setq frame-title-format "emacs [%b]")
(setq icon-title-format "emacs [%b]")
(which-func-mode)
(setq mouse-yank-at-point t)        ;; paste at cursor, not mouse location

(when (fboundp 'ffap-bindings)      ;; find-file-at-point
  (ffap-bindings))

;;; Scroll settings
(setq scroll-preserve-screen-position nil) ;; cursor stays in same place during page up/down
(setq scroll-margin 2)                     ;; start scrolling when 2 rows from top/bottom
(setq scroll-conservatively 1)             ;; smooth scrolling
(setq next-line-add-newlines nil)          ;; don't add newlines just by scrolling

(defadvice scroll-up (around ewd-scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(defadvice scroll-down (around ewd-scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))



;; ---------------------------------------------------------
;; Load utility libs
;; ---------------------------------------------------------
(add-to-list 'load-path "~/elisp")
(autoload 'find-file-recursively "find-recursive" "" t)
(autoload 'linum "linum" "" t)
(autoload 'light-symbol-mode "light-symbol" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'blank-mode "blank-mode" "Toggle blank visualisation" t)
(autoload 'typing-of-emacs "typing" "The Typing-Of-Emacs, a game" t)
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;; ---------------------------------------------------------
;; Custom keybindings
;; ---------------------------------------------------------
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;; = meta
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ;; = meta
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key [f1]  'bs-show)
(global-set-key [f2]  'bm-next)
(global-set-key [f3]  'kill-buffer)
(global-set-key [f4]  'shell)
(global-set-key [f5]  'compile)
(global-set-key [f6]  'grep-find)
(global-set-key [f7]  'find-tag)
(global-set-key [f8]  'pop-tag-mark)
(global-set-key [f9]  'split-window-vertically)
(global-set-key [f10] 'split-window-horizontally)
(global-set-key [f11] 'delete-window)
(global-set-key [f12] 'delete-other-windows)

(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>")  'windmove-left)

(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)

;; ---------------------------------------------------------
;; Backups in ~/.backups
;; ---------------------------------------------------------
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.backup")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;; ---------------------------------------------------------
;; Programming Style
;;   Java and C++ pick up c-mode-hook
;; ---------------------------------------------------------
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "K&R")
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)))

;; ---------------------------------------------------------
;; Ruby setup
;; ---------------------------------------------------------
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files")
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
             (local-set-key [f5] 'ruby-lint)))

;; ---------------------------------------------------------
;; SLIME Setup
;; ---------------------------------------------------------
(show-paren-mode)

(when (string-equal system-name "mktop")
  (setq inferior-lisp-program "/opt/bin/sbcl --noinform")
  (add-to-list 'load-path "/home/mk/.sbcl/site/slime")
  (add-to-list 'load-path "/home/mk/.sbcl/site/slime/contrib")
  (autoload 'slime "slime" "Start and connect to the inferior lisp image" t)
  (autoload 'slime-mode "slime" "Start slime-mode for this buffer" t)
  (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")
  (autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)

  (eval-after-load "slime"
    '(progn
       (slime-setup '(slime-fancy slime-asdf slime-banner slime-highlight-edits))

       (setq slime-complete-symbol*-fancy t
             slime-complete-symbol-function 'slime-fuzzy-complete-symbol
             slime-when-complete-filename-expand t
             slime-truncate-lines nil
             slime-autodoc-use-multiline-p t
             slime-startup-animation nil)

       (define-key slime-mode-map      (kbd "C-TAB")   'slime-fuzzy-complete-symbol)
       (define-key slime-repl-mode-map (kbd "C-TAB")   'slime-fuzzy-complete-symbol)
       (define-key slime-mode-map      (kbd "TAB")     'slime-indent-and-complete-symbol)
       (define-key slime-mode-map      (kbd "C-c ;")   'slime-insert-balanced-comments)
       (define-key slime-repl-mode-map (kbd "C-c ;")   'slime-insert-balanced-comments)
       (define-key slime-mode-map      (kbd "C-c M-;") 'slime-remove-balanced-comments)
       (define-key slime-repl-mode-map (kbd "C-c M-;") 'slime-remove-balanced-comments)
       (define-key slime-mode-map      (kbd "RET")     'newline-and-indent)
       (define-key slime-mode-map      (kbd "")        'newline-and-indent)
       (define-key slime-mode-map      (kbd "C-j")     'newline)
       (define-key slime-mode-map      (kbd "<f5>")    'slime-selector)
       (define-key slime-repl-mode-map (kbd "<f5>")    'slime-selector)
       (define-key slime-mode-map      (kbd "C-c r")   'goto-repl)

       (paredit-mode +1)
       (require 'ecb))))

;; do slime mode for all lisp files
(add-hook 'lisp-mode-hook (lambda ()
                            (cond ((not (featurep 'slime))
                                   (require 'slime)
                                   (normal-mode)))
			    (modify-syntax-entry ?- "w")))

;; ---------------------------------------------------------
;; NXML Setup
;; ---------------------------------------------------------
(load "~/elisp/nxml-mode/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))

;; ---------------------------------------------------------
;; Java Setup
;; ---------------------------------------------------------
(setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
(setq semantic-load-turn-useful-things-on t)
(setq semanticdb-default-save-directory "~/.semantic.cache")
(setq semanticdb-persistent-path nil)
(load-file "~/elisp/cedet-1.0pre3/common/cedet.el")
(add-to-list 'load-path "~/local/share/emacs/site-lisp/elib")
(add-to-list 'load-path "~/elisp/ecb-2.32")
(add-to-list 'load-path "~/elisp/jde-2.3.5.1/lisp")

(setq defer-loading-jde t)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
            (append
             '(("\\.java\\'" . jde-mode))
             auto-mode-alist)))
  (require 'jde))

(defun my-jde-mode-hook ()
  (global-set-key [f5] 'jde-compile)
  (require 'ecb))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

(add-hook 'java-mode-hook '(lambda ()
                             (c-set-style "java")
                             (setq c-basic-offset 3)
                             (setq-default indent-tabs-mode nil)))

(setq ecb-layout-name "left10")


;; ----------------------------------------------------------
;; Javascript
;; ----------------------------------------------------------
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;; ----------------------------------------------------------
;; etags-select
;; ----------------------------------------------------------
(autoload 'etags-select-find-tag "etags-select" "for etags-select method" t)
(setq etags-select-no-select-for-one-match nil)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key [f7] 'etags-select-find-tag)

;; ----------------------------------------------------------
;; Tramp (remote editing)
;; ----------------------------------------------------------
(setq tramp-default-method "scp")
(setq tramp-default-user-alist
               '(("scp" ".*\\.littleredbat\\.net\\" "mk")
                 ("scp" "lrb" "mk")
                 ("scp" ".*\\.nortel\\.com\\" "matthewk")
                 ("scp" "znc0y0n8.*" "matthewk")
                 ("scp" "deb" "matthewk")
                 (nil nil "matthewk")))

;; ----------------------------------------------------------
;; Git VC backend setup
;; ----------------------------------------------------------
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
(require 'git)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

;; ---------------------------------------------------------
;; org-mode
;; ---------------------------------------------------------
(add-to-list 'load-path "~/elisp/org-mode")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; ---------------------------------------------------------
;; file-local bookmarks from bm.el
;; ---------------------------------------------------------
(require 'bm)
(global-set-key (kbd "<M-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; ----------------------------------------------------------
;; my elisp functions
;; ----------------------------------------------------------
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun tag-region (element)
  "Reads element name from minibuffer and inserts start and end tags."
  (interactive "sTag region with element: ")
  (save-excursion
    (when (and element (not (equal element "")))
      (when (string-match "[ \t]*$" element)
        (setq element (replace-match "" nil nil element))) ;; trim whitespace
      (when (string-match "^[ \t]*" element)
        (setq element (replace-match "" nil nil element))) ;; trim whitespace
      (goto-char (region-end))
      (insert "</" element ">")
      (goto-char (region-beginning))
      (insert "<" element ">"))))

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun vi-open-prev-line (arg)
  "Move to the prev line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (previous-line 1)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(global-set-key [(control o)] 'vi-open-next-line)
(global-set-key [(control O)] 'vi-open-prev-line)


(defun mcp-set-proj (proj)
  (interactive "sProject name: ")
  (let* ((mcp-dir "/mcp/")
         (git-dir "/localdisk/data/matthewk/code/mcp.git/mcp/")
         (src-dir mcp-dir)
         (wrk-dir (concat "/localdisk/data/matthewk/workdir/" proj))
         (cls-dir (concat wrk-dir "/classes")))
    (when (y-or-n-p "Git project?")
      (setq src-dir git-dir))
    (if (file-directory-p src-dir)
        (progn
          (setq jde-compile-option-directory cls-dir)
          (setq jde-compile-option-sourcepath (list (concat src-dir "mcp_core_root/src")
                                                    (concat src-dir "mcp_core_root/src")
                                                    (concat src-dir "mcp_core_ims/ims")))
          (let* ((jars '("mcp_3rdParty/java/database/oracle/oracle.zip"
                         "mcp_3rdParty/java/management/jdmk/jawall.jar"
                         "mcp_3rdParty/java/megaco/megaco.jar"
                         "mcp_3rdParty/java/parsing/jdom/jdom.jar"
                         "mcp_3rdParty/java/uas/uasemClient.jar"
                         "mcp_3rdParty/java/tools/sun/tools.jar"
                         "mcp_3rdParty/java/axis/axis.jar"
                         "mcp_3rdParty/java/axis/OPIClient.jar"
                         "mcp_3rdParty/java/tomcat/tomcat.jar"
                         "mcp_3rdParty/java/httpclient/commons-httpclient-2.0-rc2.jar"
                         "mcp_3rdParty/java/management/jdmk/jsnmpapi.jar"
                         "mcp_3rdParty/java/management/jdmk/jdmkrt.jar"
                         "mcp_3rdParty/java/security/bcprov-jdk14-124.jar"
                         "mcp_3rdParty/java/masSoapServices/masservice.jar"
                         "mcp_3rdParty/java/jazzlib/jazzlib.jar"))
                 (classpath (mapcar '(lambda (j) (concat src-dir j)) jars)))
            (setq jde-global-classpath (push cls-dir classpath)))
          (setq jde-jdk-registry '(("1.5.0_07" . "/localdisk/jdk1.5.0_07")))
          (message "Change JDE paths for proj %s" proj)
          t)
      (progn
        (message "Can't read dir %s" src-dir)
        nil))))

(defun occur-at-point()
  "invokes occur with the thing-at-point"
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'other-window)))

(global-set-key "\C-cg" 'occur-at-point)

(defun goto-repl ()
  "Open the slime REPL buffer in the current window"
  (interactive)
  (when (get-buffer "*slime-repl sbcl*")
    (switch-to-buffer "*slime-repl sbcl*")))

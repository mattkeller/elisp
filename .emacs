(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(iswitchb-mode 1)                   ;; more informative buffer switching
(line-number-mode 1)
(column-number-mode 1)              ;; lines and columns, duh
(menu-bar-mode 1)                   ;; keyboard only, please
(tool-bar-mode 0)
(setq-default indent-tabs-mode nil) ;; by default, insert spaces, not a full tab
(setq visible-bell t)               ;; no beeping!
(setq next-line-add-newlines nil)   ;; don't add newlines just by scrolling
(fset 'yes-or-no-p 'y-or-n-p)       ;; query with y or n always
(setq inhibit-splash-screen t)
(setq scroll-preserve-screen-position t) ;; cursor stays in same place during page up/down
(setq scroll-margin 2)              ;; start scrolling when 2 rows from top/bottom
(setq scroll-conservatively 1)      ;; smooth scrolling
(blink-cursor-mode nil)             ;; no blinking!

;; ---------------------------------------------------------
;; Load utility libs
;; ---------------------------------------------------------
(add-to-list 'load-path "~/elisp")
(autoload 'find-file-recursively "find-recursive" "" t)
(autoload 'linum "linum" "" t)
(autoload 'light-symbol-mode "light-symbol" "" t)
(autoload 'htmlize-buffer "htmlize" "" t)
(autoload 'blank-mode "blank-mode" "Toggle blank visualisation" t)

;; ---------------------------------------------------------
;; Custom keybindings
;; ---------------------------------------------------------
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;; = meta
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ;; = meta
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-x C-b") 'bs-show)

(global-set-key [f1]  'goto-line)
(global-set-key [f2]  'save-buffer)
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
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))
(autoload 'rubydb "rubydb2x" "Ruby debugger" t)
(add-hook 'ruby-mode-hook 'turn-on-font-lock)


;; ---------------------------------------------------------
;; SLIME Setup for hendrix
;; ---------------------------------------------------------
(when (string-equal system-name "hendrix")
  (setq inferior-lisp-program "/usr/bin/sbcl --noinform")
  (add-to-list 'load-path "/home/mk/lisp/slime-2.0")
  (autoload 'slime "slime" "Start and connect to the inferior lisp image" t)
  (autoload 'slime-mode "slime" "Start slime-mode for this buffer" t)
  (eval-after-load "slime" '(slime-setup)))


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
                             (c-set-style "k&r")
                             (setq c-basic-offset 3)
                             (setq-default indent-tabs-mode nil)))


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




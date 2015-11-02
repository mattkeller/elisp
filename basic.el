;;;; basic.el - setup I ALWAYS want. Does not depend on other libs.

;;; Keep Custom settings out of .emacs ---------------------------------

(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(require 'cl)

;;; Miscellaneous customizations ---------------------------------------------

(ido-mode t)                             ; more informative buffer switching
(line-number-mode 1)                     ; show line number in mode-line
(show-paren-mode t)                      ; flash matching paren
(column-number-mode 1)                   ; lines and columns, duh
(menu-bar-mode 0)                        ; keyboard only, please
(tool-bar-mode 0)                        ; no tool bar
(scroll-bar-mode -1)
(which-func-mode t)                      ; show current fn in mode-line
(global-hl-line-mode 1)                  ; highlight current line
(setq-default indent-tabs-mode nil)      ; by default, insert spaces, not a full tab
(setq visible-bell t)                    ; no beeping!
(fset 'yes-or-no-p 'y-or-n-p)            ; query with y or n always
(setq inhibit-splash-screen t)           ; no splash screen!
(blink-cursor-mode 0)                    ; no blinking!
(auto-compression-mode 1)                ; inline edit files in gzip, bzip2 archives
(transient-mark-mode 0)                  ; don't highlight region, unless Ctrl-Space x 2
(setq help-window-select t)              ; always jump to the help window
(setq message-log-max 5000)              ; allow lots of messages
(put 'narrow-to-region 'disabled nil)    ; allow narrow-to-region
(put 'erase-buffer 'disabled nil)        ; allow erase-buffer
(put 'downcase-region 'disabled nil)     ; allow downcase-region
(put 'set-goal-column 'disabled nil)     ; allow set-goal-column
(setenv "PAGER" "/bin/cat")              ; for git in shell
(setq large-file-warning-threshold       ; tired of warning messages opening large TAGS files
      20000000)
(setq tags-revert-without-query t)       ; don't prompt to reload TAGS file

(setq iswitchb-buffer-ignore '("^ " "*Buffer" "*Messages*" "*Help*"))

(setq frame-title-format "emacs [%b]")
(setq icon-title-format "emacs [%b]")
(setq mouse-yank-at-point t)             ; paste at cursor, not mouse location

(when (fboundp 'ffap-bindings)           ; find-file-at-point
  (ffap-bindings))

(when window-system (set-background-color "gray97"))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp 'buffer-file-coding-system)
    (setq-default buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))
 
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Global keybindings -------------------------------------------------

;; these are overridden in smex config
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; = meta-x
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; = meta-x

(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "C-x C-p") 'ffap)
(global-set-key (kbd "<C-tab>") 'other-window)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)  ; use regexp version
(global-set-key (kbd "C-r") 'isearch-backward-regexp) ; use regexp version
(global-set-key (kbd "M-%") 'query-replace-regexp)    ; use regexp version
(global-set-key (kbd "C-c v") 'revert-buffer)

(global-set-key (kbd "C-x m")   (lambda () (interactive) (message "Mail? No thanks.")))

(global-set-key [f1]  'ibuffer)
(global-set-key [f2]  'mk-shell-dwim)                        ; C-u F2 => start new shell TODO: customization
(global-set-key [f3]  'kmacro-start-macro-or-insert-counter) ; emacs std
(global-set-key [f4]  'kmacro-end-or-call-macro)             ; emacs std
(global-set-key [f5]  'compile)
(global-set-key [f6]  'grep-find)
(global-set-key [f7]  'find-tag)
(global-set-key [f8]  'pop-tag-mark)
(global-set-key [f9]  'split-window-vertically)
(global-set-key [f10] 'split-window-horizontally)
(global-set-key [f11] 'global-hl-line-mode)
(global-set-key [f12] 'revert-buffer)

(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>")  'windmove-left)

(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-?") 'ispell-complete-word)

(global-set-key (kbd "M-<down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-<up>") (lambda () (interactive) (scroll-down 1)))

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;;;; Aliases ------------------------------------------------------------

(defalias 'qrr 'query-replace-regexp)
(defalias 'rvt 'revert-buffer)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'bml 'bookmark-bmenu-list)
(defalias 'bmj 'bookmark-jump)
(defalias 'bms 'bookmark-set)
(defalias 'ptr 'point-to-register)
(defalias 'jtr 'jump-to-register)
(defalias 'rct 'recentf-open-files)
(defalias 'ttl 'toggle-truncate-lines)

;;; Scroll settings ----------------------------------------------------

(setq scroll-preserve-screen-position nil) ; cursor stays in same place during page up/down
(setq scroll-margin 2)                     ; start scrolling when 2 rows from top/bottom
(setq scroll-conservatively 1)             ; smooth scrolling
(setq next-line-add-newlines nil)          ; don't add newlines just by scrolling
;(set-scroll-bar-mode 'right)               ; scroll bar on right of buffer

(defadvice scroll-up (around scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

(defadvice scroll-down (around scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

;;; Compile mode settings ----------------------------------------------

(setq compilation-read-command t)       ; always ask for compile command
(setq compilation-scroll-output t)      ; scroll compilation window

; make compiler error clickable in shell
(add-hook 'shell-mode-hook
          (lambda () (compilation-shell-minor-mode))) 

;;; Backups in ~/.backups ----------------------------------------------

(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.backup")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 2              ; Number of newest versions to keep
      kept-old-versions 5              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;;; Dired --------------------------------------------------------------

(defun turn-on-auto-revert-mode ()
  (interactive)
  (auto-revert-mode 1))
 
(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)

;;; Programming Style: Java and C++ pick up c-mode-hook ----------------

(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "K&R")
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (linum-mode)))

;;; EDiff --------------------------------------------------------------

(if (locate-library "ediff")
    (progn
      (autoload 'ediff-files "ediff")
      (autoload 'ediff-buffers "ediff")

      (eval-after-load "ediff" '(progn
                                  (message "doing ediff customisation")
                                  (setq diff-switches  "-u"
                                        ediff-custom-diff-options  "-U3"
                                        ediff-split-window-function 'split-window-horizontally
                                        ediff-window-setup-function 'ediff-setup-windows-plain)

                                  (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
                                  (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)))))

;;; hippie-expand fun --------------------------------------------------

(setq hippie-expand-try-functions-list 
      '(try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-line
        try-expand-list
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;; Autoloads ----------------------------------------------------------

(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

;;; Utils --------------------------------------------------------------

(defun mk-shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.

 If a buffer with a running shell process exists, simply switch
 to that buffer. If a shell buffer exists, but the shell process
 is not running, restart the shell. If already in an active shell
 buffer, switch to the next one, if any. With prefix argument,
 CREATE a new shell."
  (interactive "P")
  (let* ((next-shell-buffer
          (catch 'found
            (dolist (buffer (reverse (buffer-list)))
              (when (string-match "^\\*shell\\*" (buffer-name buffer))
                (throw 'found buffer)))))
         (buffer (if create
                     (generate-new-buffer-name "*shell*")
                   next-shell-buffer)))
    (shell buffer)
    buffer))

(provide 'basic)

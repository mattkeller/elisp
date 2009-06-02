;;;; mcp-trace.el -- major mode to aid reading & navigating mcp trace files

;; TODO:
;; * Move through trace by: next-msg, prev-msg
;; * Highlight keywords to aid readability:
;;    SIP users (bob@domain)
;; * Show that we are hiding (or not) transactor traces

(defvar mcp-trace-mode-hook nil
  "*List of functions to call when entering mcp-trace mode.")

;;; SIP font-lock ------------------------------------------------------

(make-face 'mcp-trace-sip-face)
(copy-face 'default 'mcp-trace-sip-face)
(set-face-attribute 'mcp-trace-sip-face nil :weight 'bold)
(set-face-attribute 'mcp-trace-sip-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-sip-face nil :foreground "dark green")

(defvar mcp-trace-section-headers '("SIP Message Trace" "Incoming GCP Message" "Outgoing GCP Message"
                                    "Transactor IDLE" "Transactor PROCESS" "Transactor SUSPEND"))

(defvar mcp-trace-sip-methods '("INVITE" "ACK" "BYE" "CANCEL" "INFO" "UPDATE" "PING"  "OPTIONS" "REGISTER"))

(defvar mcp-trace-sip-responses '("100 Trying" "180 Ringing" "183 Session Progress" "200 OK"
                                  "200 Registration Successful" "401 Unauthorized"
                                  "407 Proxy Authentication Required" "408 Request Timeout"
                                  "480 Temporarily not available" "486 Busy Here"))

;;; GCP font-lock ------------------------------------------------------

(defvar mcp-trace-gcp-methods '("LocalNewCall" "RemoteNewCall" "LocalAck" "RemoteAck" "LocalAlerting" "RemoteAlerting"
                                "LocalAnswer" "RemoteAnswer" "LocalHold" "RemoteHold" "LocalApplySignal" "RemoteApplySignal"
                                "LocalFeatureNotify" "RemoteFeatureNotify" "LocalRelease" "RemoteRelease" "LocalReleaseComp"
                                "RemoteReleaseComp" "LocalRestore" "RemoteRestore"))

(make-face 'mcp-trace-gcp-face)
(copy-face 'default 'mcp-trace-gcp-face)
(set-face-attribute 'mcp-trace-gcp-face nil :weight 'bold)
(set-face-attribute 'mcp-trace-gcp-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-gcp-face nil :foreground "orange red")

;;; Signal font-lock ---------------------------------------------------

(defvar mcp-trace-signals '("SignalOpen" "SignalProgress" "SignalClose" "SignalResponse" "SignalFeature" "PersistenceResponseSignal"))

(defvar mcp-trace-sip-signals (mapcar (lambda (s) (concat "Sip" s)) mcp-trace-signals))

(defvar mcp-trace-gcp-signals (mapcar (lambda (s) (concat "GCP" s)) mcp-trace-signals))

(make-face 'mcp-trace-signal-face)
(copy-face 'default 'mcp-trace-signal-face)
(set-face-attribute 'mcp-trace-signal-face nil :weight 'normal)
(set-face-attribute 'mcp-trace-signal-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-signal-face nil :foreground "midnight blue")

(make-face 'mcp-trace-sip-signal-face)
(copy-face 'default 'mcp-trace-sip-signal-face)
(set-face-attribute 'mcp-trace-sip-signal-face nil :weight 'normal)
(set-face-attribute 'mcp-trace-sip-signal-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-sip-signal-face nil :foreground "dark green")

(make-face 'mcp-trace-gcp-signal-face)
(copy-face 'default 'mcp-trace-gcp-signal-face)
(set-face-attribute 'mcp-trace-gcp-signal-face nil :weight 'normal)
(set-face-attribute 'mcp-trace-gcp-signal-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-gcp-signal-face nil :foreground "orange red")

;;; Transactor font-lock -----------------------------------------------

(defvar mcp-trace-trans-keywords '("IPTelASE" "CallMgrASE" "IWCMSvcASE" "MODEL: OCM" "MODEL: TCM"))

(make-face 'mcp-trace-trans-face)
(copy-face 'default 'mcp-trace-trans-face)
(set-face-attribute 'mcp-trace-trans-face nil :weight 'bold)
(set-face-attribute 'mcp-trace-trans-face nil :slant 'italic)
(set-face-attribute 'mcp-trace-trans-face nil :foreground "midnight blue")


(defvar mcp-trace-font-lock-keywords
  `((,(regexp-opt mcp-trace-sip-methods      'words) . 'mcp-trace-sip-face)
    (,(regexp-opt mcp-trace-gcp-methods      'words) . 'mcp-trace-gcp-face)
    (,(regexp-opt mcp-trace-sip-responses    'words) . 'mcp-trace-sip-face)
    (,(regexp-opt mcp-trace-signals          'words) . 'mcp-trace-signal-face)
    (,(regexp-opt mcp-trace-sip-signals      'words) . 'mcp-trace-sip-signal-face)
    (,(regexp-opt mcp-trace-gcp-signals      'words) . 'mcp-trace-gcp-signal-face)
    (,(regexp-opt mcp-trace-trans-keywords   'words) . 'mcp-trace-trans-face)
    (,(regexp-opt mcp-trace-section-headers)         . 'bold)))

(defun mcp-trace-mode ()
  "Major mode for editing mcp-trace files."
   (interactive)
   (kill-all-local-variables)

   ;; define pages as indidual msgs or the transactor traces processing those msgs
  (make-local-variable 'page-delimiter)
  (setq page-delimiter (concat "^" (regexp-opt mcp-trace-section-headers)))
  ;; move by page: C-x ], C-x [
  ;; narrow to page: C-x n p
  
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(mcp-trace-font-lock-keywords nil t))

   (setq major-mode 'mcp-trace-mode)
   (setq mode-name "mcp-trace")
   (run-hooks 'mcp-trace-mode-hook))

(defun mcp-trace-hide-transactors ()
  "Hide all the transactor traces"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^Transactor " (point-max) t)
      (let* ((start (- (point) 11))
             (end (re-search-forward "\n \n" (point-max) t))) ; TODO: hope no one removes that extra space
        (when (and start end)
          (let ((ovl (make-overlay start end)))
            (overlay-put ovl 'invisible t)))))))

(defun mcp-trace-show-transactors ()
  "Show all the transactor traces"
  (interactive)
  (dolist (ovl (overlays-in (point-min) (point-max)))
    (overlay-put ovl 'invisible nil)
    (delete-overlay ovl))
  (setq mcp-trace-overlays nil))

(defun mcp-trace-toggle-show-transactors ()
  "Toggle transcator trace visibility"
  (interactive)
  (if (overlays-in (point-min) (point-max))
      (mcp-trace-show-transactors)
    (mcp-trace-hide-transactors)))

(provide 'mcp-trace)

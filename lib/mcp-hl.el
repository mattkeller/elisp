;;;; mcp-hl.el -- minor mode to fontify MCP SSL trace files

;; Usage:
;; (autoload 'mcp-hl-mode "mcp-hl" "" t)
;; M-x mcp-hl-mode

;;; SIP font-lock ------------------------------------------------------

(defvar mcp-hl-sip-methods '("INVITE" "ACK" "BYE" "CANCEL" "INFO" "UPDATE"
                             "PING"  "OPTIONS" "\\bREGISTER\\b"))

(defvar mcp-hl-sip-responses '("100 Trying" "180 Ringing" "183 Session Progress"
                               "200 OK" "200 Registration Successful" "401 Unauthorized"
                               "407 Proxy Authentication Required" "408 Request Timeout"
                               "480 Temporarily not available" "486 Busy Here"))

;;; GCP font-lock ------------------------------------------------------

(defvar mcp-hl-gcp-methods '("LocalNewCall" "RemoteNewCall" "LocalAck"
                             "RemoteAck" "LocalAlerting" "RemoteAlerting"
                             "LocalAnswer" "RemoteAnswer" "LocalHold"
                             "RemoteHold" "LocalApplySignal" "RemoteApplySignal"
                             "LocalFeatureNotify" "RemoteFeatureNotify"
                             "LocalRelease" "RemoteRelease" "LocalReleaseComp"
                             "RemoteReleaseComp" "LocalRestore" "RemoteRestore"))

;;; Signal font-lock ---------------------------------------------------

(defvar mcp-hl-signals '("SignalOpen" "SignalProgress" "SignalClose"
                         "SignalResponse" "SignalFeature" "PersistenceResponseSignal"))

(defvar mcp-hl-sip-signals (mapcar (lambda (s) (concat "Sip" s))
                                      mcp-hl-signals))

(defvar mcp-hl-gcp-signals (mapcar (lambda (s) (concat "GCP" s))
                                      mcp-hl-signals))

;;; Transactor font-lock -----------------------------------------------

(defvar mcp-hl-trans-keywords '("IPTelASE" "CallMgrASE" "IWCMSvcASE"
                                "MODEL: OCM" "MODEL: TCM"))

;;; Assign faces to keywords -------------------------------------------

(defvar mcp-hl-font-lock-keywords
  `((,(regexp-opt mcp-hl-sip-methods      'words) . 'font-lock-type-face)
    (,(regexp-opt mcp-hl-gcp-methods      'words) . 'font-lock-type-face)
    (,(regexp-opt mcp-hl-sip-responses    'words) . 'font-lock-type-face)
    (,(regexp-opt mcp-hl-signals          'words) . 'font-lock-keyword-face)
    (,(regexp-opt mcp-hl-sip-signals      'words) . 'font-lock-keyword-face)
    (,(regexp-opt mcp-hl-gcp-signals      'words) . 'font-lock-keyword-face)
    (,(regexp-opt mcp-hl-trans-keywords   'words) . 'font-lock-string-face)))

(define-minor-mode mcp-hl-mode
  "mcp-hl provides extra syntax highlighting for MCP trace files"
  :lighter " mcp-hl"
  (if mcp-hl-mode
      (font-lock-add-keywords nil mcp-hl-font-lock-keywords)
    (font-lock-remove-keywords nil mcp-hl-font-lock-keywords))
  (font-lock-fontify-buffer))

(provide 'mcp-hl)

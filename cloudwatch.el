;;; cloudwatch.el --- AWS CloudWatch log viewer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 @rand-fu
;;
;; Author: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Maintainer: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Created: November 04, 2025
;; Modified: March 13, 2026
;; Version: 0.6.0
;; Keywords: tools aws cloudwatch logs monitoring devops kubernetes observability
;; Homepage: https://github.com/rand-fu/cloudwatch-el
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licensed under the GNU General Public License v3.0 or later.
;; See the LICENSE file in the project root for full license text.
;;
;;; Commentary:
;;
;; An Emacs interface for browsing AWS CloudWatch logs without leaving
;; your editor.
;;
;; Features:
;; - Interactive transient-based interface for easy navigation
;; - Live log tailing with automatic updates
;; - Snapshot queries for historical log analysis
;; - CloudWatch Insights support for advanced queries and aggregations
;; - Filter patterns with quick presets for common searches
;; - Relative (minutes back) or absolute (date range) time selection
;; - Region switching for multi-region deployments
;; - Favorites management for frequently accessed log groups
;; - Syntax highlighting for log levels (ERROR, WARN, INFO, DEBUG)
;; - Asynchronous operations to keep Emacs responsive
;;
;; Usage:
;;   M-x cloudwatch RET
;;
;; Requires AWS CLI installed and configured with appropriate credentials.
;; See README.md for detailed configuration and examples.
;;
;;; Code:

;;;; Requirements
(require 'transient)
(require 'ansi-color)
(require 'json) ;; for json-pretty-print

;;;; Customization group
(defgroup cloudwatch nil
  "AWS CloudWatch log viewer."
  :group 'tools)

;;;; Custom vars
(defcustom cloudwatch-default-region "us-west-2"
  "Default AWS region for CloudWatch logs."
  :type 'string
  :group 'cloudwatch)

(defcustom cloudwatch-favorite-log-groups nil
  "List of frequently used CloudWatch log groups.
Example: \='(\"/aws/containerinsights/prod/application\"
           \"/aws/lambda/my-function\")"
  :type '(repeat string)
  :group 'cloudwatch)

(defcustom cloudwatch-query-limit 2500
  "Maximum number of log events to retrieve in basic query mode.
Does not affect CloudWatch Insights - use \='limit\=' in the query itself."
  :type 'integer
  :group 'cloudwatch)

(defcustom cloudwatch-insights-column-widths
  '(("@timestamp" . 26)
    ("@logStream" . 50)
    ("log" . 100)
    ("@message" . 100)
    ("message" . 100)
    ("kubernetes.pod_name" . 50)
    ("@ptr" . 15)
    ("@id" . 15)
    ("@requestId" . 15)
    (default . 30))
  "Column widths for CloudWatch Insights results display.
Keys are field names, values are character widths.
The special key `default' sets the width for unlisted fields.
These are base widths - `cloudwatch-wide-mode' can double message fields."
  :type '(alist :key-type (choice string (const default))
          :value-type integer)
  :group 'cloudwatch)

;; These are generalized examples, usefulness depends on users log format and needs. We can probably improve them for a wider audience.
(defcustom cloudwatch-insights-presets
  '(("Top 10 slowest requests" . "fields @timestamp, @message | filter duration > 0 | sort duration desc | limit 10")
    ("Errors by count" . "fields @message | filter @message like /ERROR/ | stats count() by bin(5m)")
    ("Request rate" . "fields @timestamp | stats count() by bin(1m) as requests_per_minute")
    ("5xx errors timeline" . "fields @timestamp | filter statusCode >= 500 | stats count() by bin(5m)")
    ("Memory usage stats" . "fields @timestamp, memory_used | stats avg(memory_used), max(memory_used), min(memory_used) by bin(5m)")
    ("Unique users" . "fields userId | stats count_distinct(userId) as unique_users by bin(1h)")
    ("Pod restarts" . "filter @message like /restarting/ | stats count() by kubernetes.pod_name"))
  "Preset CloudWatch Insights queries."
  :type '(alist :key-type string :value-type string)
  :group 'cloudwatch)

;; If an Insights query gets stuck in "Running" state which AWS does sometimes, the timer chain never stops. If a user kills the buffer while "running", the timer fires into a dead buffer and errors out.
(defcustom cloudwatch-insights-poll-timeout 120
  "Maximum seconds to poll for Insights query results before giving up."
  :type 'integer
  :group 'cloudwatch)

;;;; Internal vars
(defvar cloudwatch-current-region nil
  "Currently active AWS region for CloudWatch operations.
Initialized lazily from `cloudwatch-default-region'.")

(defvar cloudwatch-current-log-group nil
  "Currently selected CloudWatch log group.")

(defvar cloudwatch-current-minutes 5
  "Number of minutes to look back when querying or tailing logs.")

(defvar cloudwatch-current-filter ""
  "Current filter pattern for log queries.
Can be a simple text pattern or CloudWatch JSON filter syntax.")

(defvar cloudwatch-log-groups-cache nil
  "Cached list of log groups for the current region.
Automatically refreshed when region changes or cache expires.")

(defvar cloudwatch-cache-time nil
  "Timestamp when log groups cache was last updated.
Used to determine if cache needs refresh (10 minute expiry).")

(defvar cloudwatch-history nil
  "History of log group selections.")

(defvar cloudwatch-wide-mode nil
  "When non-nil, double the width of message fields in Insights results.")

(defvar cloudwatch-insights-history nil
  "History of CloudWatch Insights queries.")

(defvar cloudwatch-insights-query nil
  "Current CloudWatch Insights query string.")

(defvar cloudwatch-time-mode 'relative
  "Time selection mode: `relative' (minutes back) or `absolute' (date range).")

(defvar cloudwatch-start-time nil
  "Absolute start time as Emacs time value.")

(defvar cloudwatch-end-time nil
  "Absolute end time as Emacs time value.")

;; Literally just so we can have a key that evil-mode doesn't take over for expanding the time.
(defvar cloudwatch-insights-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "g") 'cloudwatch-rerun-insights)
    (define-key map (kbd "+") 'cloudwatch-insights-expand-time)
    (define-key map (kbd "RET") 'cloudwatch-insights-show-detail)
    map)
  "Keymap for CloudWatch Insights results.")

(defvar cloudwatch-multiline-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c C-c") #'exit-minibuffer)
    (define-key map (kbd "RET") #'newline)
    map)
  "Keymap for multiline query input.
RET inserts a newline, \\[cloudwatch-multiline-map] submits.")

;;;; Buffer locals
(defvar-local cloudwatch-insights-query-info nil
  "Buffer-local storage for Insights query metadata and parameters.
Used for refreshing queries with \='g\=' and expanding time range with \='G\='.")

(defvar-local cloudwatch-insights-results nil
  "Buffer-local storage for Insights query results.")

;; Because we do care about UX in Emacs!
(defmacro cloudwatch-with-transient-fallback (&rest body)
  "Execute BODY, returning to transient menu on user-error or quit.
This provides a better UX by keeping the transient open when
validation fails or user cancels, instead of leaving them stranded."
  `(condition-case err
       (progn ,@body)
     (user-error
      (message "%s" (error-message-string err))
      (sit-for 1.5)
      (cloudwatch-transient))
     (quit
      (cloudwatch-transient))))

;; Minor mode
(define-minor-mode cloudwatch-insights-results-mode
  "Minor mode for CloudWatch Insights results buffer."
  :lighter " CW-Insights"
  :keymap cloudwatch-insights-results-mode-map)

;;;; Helper functions
(defun cloudwatch-get-region ()
  "Get current region, initializing from default if needed."
  (or cloudwatch-current-region
      (setq cloudwatch-current-region cloudwatch-default-region)))

;; An attempt at error handling - this could be too restrictive?
(defun cloudwatch--run-aws-command (cmd &optional silent)
  "Run AWS CLI CMD and return output, handling errors gracefully.
If SILENT is non-nil, don't show error messages for expected failures."
  (let ((output (shell-command-to-string (concat cmd " 2>&1"))))
    (cond
     ((string-match "Unable to locate credentials" output)
      (user-error "AWS credentials not configured. Run 'aws configure' or set environment variables"))
     ((string-match "ExpiredToken" output)
      (user-error "AWS session token expired. Please refresh your credentials"))
     ((string-match "AccessDenied\\|UnauthorizedAccess" output)
      (user-error "Access denied. Check your IAM permissions for CloudWatch Logs"))
     ((string-match "ResourceNotFoundException" output)
      (unless silent
        (user-error "Log group not found: %s" cloudwatch-current-log-group))
      nil)
     ((string-match "InvalidParameterException\\|ValidationException" output)
      (user-error "Invalid query or parameters: %s"
                  (if (string-match "message.*?:\\s-*\\([^\n]+\\)" output)
                      (match-string 1 output)
                    output)))
     ((string-match "ThrottlingException\\|LimitExceededException" output)
      (user-error "AWS rate limit exceeded. Please wait and try again"))
     ;; Success - return the output
     (t output))))

(defun cloudwatch--get-column-width (field)
  "Get the display width for FIELD name.
Respects `cloudwatch-insights-column-widths' and `cloudwatch-wide-mode'."
  (let ((base-width (or (cdr (assoc field cloudwatch-insights-column-widths))
                        (cdr (assoc 'default cloudwatch-insights-column-widths))
                        30)))
    ;; Double message fields in wide mode
    (if (and cloudwatch-wide-mode
             (member field '("log" "@message" "message")))
        (* base-width 2)
      base-width)))

(defun cloudwatch--extract-buffer-name (log-group &optional mode)
  "Extract meaningful buffer name from LOG-GROUP path and optional MODE."
  (let ((prefix (if (eq mode 'query) "CW-Query" "CW")))
    (cond
     ((string-match "/aws/containerinsights/\\([^/]+\\)/\\([^/]+\\)" log-group)
      (format "*%s:%s:%s-%s*"
              prefix
              (cloudwatch-get-region)
              (match-string 1 log-group)
              (match-string 2 log-group)))
     ((string-match "/aws/lambda/\\([^/]+\\)" log-group)
      (format "*%s:%s:lambda-%s*"
              prefix
              (cloudwatch-get-region)
              (match-string 1 log-group)))
     (t (let ((parts (split-string log-group "/")))
          (format "*%s:%s:%s*"
                  prefix
                  (cloudwatch-get-region)
                  (string-join (last parts 2) "-")))))))

(defun cloudwatch--setup-highlighting ()
  "Setup common highlighting patterns."
  (font-lock-mode 1)
  ;; Use font-lock-add-keywords instead of highlight-regexp
  (font-lock-add-keywords nil
                          '(("\\bERROR\\b\\|\"ERROR\"\\|\"error\"\\|:ERROR:\\|\\[ERROR\\]" . 'hi-red-b)
                            ("\\bFATAL\\b\\|\"FATAL\"\\|\"fatal\"\\|\\[FATAL\\]" . 'hi-red-b)
                            ("\\bException\\b\\|\"exception\"" . 'hi-red-b)
                            ("\\bWARN\\b\\|\\bWARNING\\b\\|\"WARN\"\\|\"warn\"\\|\\[WARN\\]\\|\\[WARNING\\]" . 'hi-yellow)
                            ("\\bINFO\\b\\|\"INFO\"\\|\"info\"\\|\\[INFO\\]" . 'hi-green)
                            ("\\bDEBUG\\b\\|\"DEBUG\"\\|\"debug\"\\|\\[DEBUG\\]" . 'hi-blue)
                            ("\"[^\"]+\":" . 'font-lock-keyword-face)
                            ("[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" . 'font-lock-comment-face)))
  (font-lock-flush)
  (font-lock-ensure))

;;; Absolute date range
;; Trying to keep this simple but make it useful...

;; parse-time-string seems fragile. It handles "YYYY-MM-DD HH:MM" fine but can silently produce garbage on unexpected formats. Try to guard against it.
(defun cloudwatch--parse-time (str)
  "Parse STR as local time, erroring on invalid input."
  (let ((parsed (parse-time-string str)))
    (unless (and (nth 1 parsed) (nth 2 parsed)  ; min, hour
                 (nth 3 parsed) (nth 4 parsed) (nth 5 parsed))  ; day, month, year
      (user-error "Invalid time format: %s (expected YYYY-MM-DD HH:MM)" str))
    (encode-time 0
                 (nth 1 parsed)   ; minute
                 (nth 2 parsed)   ; hour
                 (nth 3 parsed)   ; day
                 (nth 4 parsed)   ; month
                 (nth 5 parsed))));; year

;; A way to set the time range
(defun cloudwatch-set-time-range ()
  "Set an absolute time range for queries."
  (interactive)
  (cloudwatch-with-transient-fallback
   (let* ((start-str (read-string "Start (YYYY-MM-DD HH:MM): "
                                  (format-time-string "%Y-%m-%d %H:%M"
                                                      (time-subtract nil (* cloudwatch-current-minutes 60)))))
          (end-str (read-string "End (YYYY-MM-DD HH:MM): "
                                (format-time-string "%Y-%m-%d %H:%M"))))
     (setq cloudwatch-time-mode 'absolute
           cloudwatch-start-time (cloudwatch--parse-time start-str)
           cloudwatch-end-time (cloudwatch--parse-time end-str)))
   (cloudwatch-transient)))

(defun cloudwatch-clear-time-range ()
  "Switch back to relative time mode."
  (interactive)
  (setq cloudwatch-time-mode 'relative
        cloudwatch-start-time nil
        cloudwatch-end-time nil)
  (cloudwatch-transient))

;; Helper for start times
(defun cloudwatch--start-time-millis ()
  "Get query start time in milliseconds."
  (if (eq cloudwatch-time-mode 'absolute)
      (* (truncate (float-time cloudwatch-start-time)) 1000)
    (* (- (truncate (float-time)) (* cloudwatch-current-minutes 60)) 1000)))

;; Helper for end times
(defun cloudwatch--end-time-millis ()
  "Get query end time in milliseconds."
  (if (eq cloudwatch-time-mode 'absolute)
      (* (truncate (float-time cloudwatch-end-time)) 1000)
    (* (truncate (float-time)) 1000)))

;; I am friendly hu-man
(defun cloudwatch--time-description ()
  "Return a human-readable description of the current time range."
  (if (eq cloudwatch-time-mode 'absolute)
      (format "%s → %s"
              (format-time-string "%Y-%m-%d %H:%M" cloudwatch-start-time)
              (format-time-string "%Y-%m-%d %H:%M" cloudwatch-end-time))
    (format "Last %d minutes" cloudwatch-current-minutes)))

(defun cloudwatch--read-multiline (prompt &optional initial)
  "Read a multiline string from the minibuffer.
PROMPT is displayed.  INITIAL is the starting text.
RET inserts a newline.  \\[cloudwatch--read-multiline] submits."
  (let ((result (read-from-minibuffer
                 (concat prompt " (RET=newline, C-c C-c=submit): ")
                 initial
                 cloudwatch-multiline-map)))
    (string-trim result)))

;;;; AWS CLI operations
(defun cloudwatch-list-log-groups (&optional refresh)
  "List all log groups in current region. Use cache unless REFRESH is true."
  (when (or refresh
            (null cloudwatch-log-groups-cache)
            (null cloudwatch-cache-time)
            (> (- (float-time) cloudwatch-cache-time) 600))
    (message "Fetching log groups from %s..." (cloudwatch-get-region))
    (let* ((cmd (format "aws logs describe-log-groups --region %s --query 'logGroups[].logGroupName' --output text"
                        (cloudwatch-get-region)))
           (output (cloudwatch--run-aws-command cmd)))
      (if (and output (not (string-empty-p (string-trim output))))
          (progn
            (setq cloudwatch-log-groups-cache
                  (split-string output "\t\\|\n" t))
            (setq cloudwatch-cache-time (float-time))
            (message "Found %d log groups" (length cloudwatch-log-groups-cache)))
        (setq cloudwatch-log-groups-cache nil)
        (message "No log groups found or error occurred"))))
  cloudwatch-log-groups-cache)

;;;; Settings functions for Transient
(defun cloudwatch-set-region ()
  "Set AWS region."
  (interactive)
  (setq cloudwatch-current-region
        (completing-read "AWS Region: "
                         '("us-west-1" "us-west-2" "us-east-1" "us-east-2"
                           "eu-west-1" "eu-central-1" "ap-southeast-1" "ap-northeast-1")
                         nil nil (cloudwatch-get-region)))
  ;; Clear cache when region changes
  (setq cloudwatch-log-groups-cache nil)
  (cloudwatch-transient))

(defun cloudwatch-set-log-group ()
  "Set the log group from favorites or type custom."
  (interactive)
  (cloudwatch-with-transient-fallback
   (setq cloudwatch-current-log-group
         (completing-read "Log group (favorites + custom): "
                          cloudwatch-favorite-log-groups
                          nil nil nil 'cloudwatch-history))
   (cloudwatch-transient)))

(defun cloudwatch-browse-log-groups ()
  "Browse and select from all log groups in region."
  (interactive)
  (cloudwatch-with-transient-fallback
   (let ((log-groups (cloudwatch-list-log-groups)))
     (unless log-groups
       (user-error "No log groups found in region %s" (cloudwatch-get-region)))
     (setq cloudwatch-current-log-group
           (completing-read (format "Select log group (%d available): " (length log-groups))
                            log-groups
                            nil t nil 'cloudwatch-history)))
   (cloudwatch-transient)))

(defun cloudwatch-refresh-cache ()
  "Refresh the log groups cache."
  (interactive)
  (cloudwatch-list-log-groups t)
  (message "Cache refreshed!")
  (cloudwatch-transient))

(defun cloudwatch-set-minutes ()
  "Set minutes to look back."
  (interactive)
  (cloudwatch-with-transient-fallback
   (setq cloudwatch-current-minutes
         (read-number "Minutes back: " cloudwatch-current-minutes))
   (cloudwatch-transient)))

(defun cloudwatch-set-filter ()
  "Set filter pattern."
  (interactive)
  (cloudwatch-with-transient-fallback
   (setq cloudwatch-current-filter
         (read-string "Filter pattern: " cloudwatch-current-filter))
   (cloudwatch-transient)))

(defun cloudwatch-set-query-limit ()
  "Set the query result limit."
  (interactive)
  (cloudwatch-with-transient-fallback
   (setq cloudwatch-query-limit
         (read-number "Maximum events to retrieve: " cloudwatch-query-limit))
   (cloudwatch-transient)))

(defun cloudwatch-toggle-wide-mode ()
  "Toggle wide column mode for Insights results."
  (interactive)
  (setq cloudwatch-wide-mode (not cloudwatch-wide-mode))
  (message "Wide mode %s (message fields %s)"
           (if cloudwatch-wide-mode "enabled" "disabled")
           (if cloudwatch-wide-mode "doubled" "normal"))
  (cloudwatch-transient))

(defun cloudwatch-set-namespace-filter ()
  "Set Kubernetes namespace filter."
  (interactive)
  (cloudwatch-with-transient-fallback
   (let ((namespace (read-string "Namespace: ")))
     (setq cloudwatch-current-filter
           (format "{ $.kubernetes.namespace_name = \"%s\" }" namespace)))
   (cloudwatch-transient)))

(defun cloudwatch-set-pod-filter ()
  "Set pod name filter with match type selection."
  (interactive)
  (cloudwatch-with-transient-fallback
   (let* ((pod (read-string "Pod name: "))
          (match-type (completing-read "Match type: "
                                       '("contains" "exact" "starts-with")
                                       nil t nil nil "contains")))
     (setq cloudwatch-current-filter
           (pcase match-type
             ("exact" (format "{ $.kubernetes.pod_name = \"%s\" }" pod))
             ("contains" (format "{ $.kubernetes.pod_name = \"*%s*\" }" pod))
             ("starts-with" (format "{ $.kubernetes.pod_name = \"%s*\" }" pod)))))
   (cloudwatch-transient)))

;;;; Standard query operations
(defun cloudwatch-do-query ()
  "Query logs from the past N minutes without tailing."
  (interactive)
  (let* ((buffer-name (cloudwatch--extract-buffer-name cloudwatch-current-log-group 'query))
         (filter-args (if (and cloudwatch-current-filter
                               (not (string-empty-p cloudwatch-current-filter)))
                          (format " --filter-pattern %s" (shell-quote-argument cloudwatch-current-filter))
                        ""))
         (cmd (format "aws logs filter-log-events --log-group-name %s --region %s --start-time %d --limit %d --output text%s"
                      (shell-quote-argument cloudwatch-current-log-group)
                      (shell-quote-argument (cloudwatch-get-region))
                      (cloudwatch--start-time-millis)
                      cloudwatch-query-limit
                      filter-args))
         (process-environment (cons "AWS_PAGER="
                                    (cons "TERM=dumb"
                                          process-environment))))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((output-buffer (get-buffer-create buffer-name)))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert (format "Querying: %s\nRegion: %s\nTime range: %s\nLimit: %d events\n"
                        cloudwatch-current-log-group
                        (cloudwatch-get-region)
                        (cloudwatch--time-description)
                        cloudwatch-query-limit))
        (when (not (string-empty-p cloudwatch-current-filter))
          (insert (format "Filter: %s\n" cloudwatch-current-filter)))
        (insert "─────────────────────────────────────────────────\n")
        (insert "⏳ Loading logs...\n")
        (toggle-truncate-lines 1)
        (local-set-key (kbd "q") 'kill-current-buffer)
        (local-set-key (kbd "g") 'cloudwatch-requery)
        (local-set-key (kbd "+") 'cloudwatch-increase-limit)
        (local-set-key (kbd "-") 'cloudwatch-decrease-limit))
      (switch-to-buffer output-buffer)
      ;; Run async
      (let ((proc (start-process-shell-command
                   "cloudwatch-query"
                   output-buffer
                   cmd)))
        (set-process-sentinel
         proc
         (lambda (process event)
           (when (string-match-p "finished\\|exited" event)
             (with-current-buffer (process-buffer process)
               (save-excursion
                 (goto-char (point-min))
                 (when (search-forward "⏳ Loading logs..." nil t)
                   (replace-match (format "✓ Query complete (max %d events)" cloudwatch-query-limit)))
                 ;; Not all of us are an advanced AI, clean up output lines for our fellow humans
                 (goto-char (point-min))
                 (while (re-search-forward "^EVENTS\t[^\t]*\t\\([0-9]+\\)\t[^\t]*\t" nil t)
                   (let* ((millis (string-to-number (match-string 1)))
                          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"
                                                         (seconds-to-time (/ millis 1000.0)))))
                     (replace-match (concat timestamp "  "))))
                 ;; Check if we hit the limit
                 (goto-char (point-max))
                 (let ((line-count (count-lines (point-min) (point-max))))
                   (when (>= line-count (+ cloudwatch-query-limit 5))
                     (goto-char (point-max))
                     (insert "\n⚠️  Result limit reached. Press '+' to increase limit and requery.")))
                 (goto-char (point-min)))
               (cloudwatch--setup-highlighting)
               (read-only-mode 1)
               (message "Query complete. Press 'g' to refresh, '+/-' to adjust limit, 'q' to quit.")))))
        (message "Querying logs asynchronously (limit: %d)..." cloudwatch-query-limit)))))

(defun cloudwatch-do-query-safe ()
  "Execute query command, returning to transient on validation errors."
  (interactive)
  (cloudwatch-with-transient-fallback
   (unless cloudwatch-current-log-group
     (user-error "Please select a log group first"))
   (cloudwatch-do-query)))

(defun cloudwatch-requery ()
  "Rerun the query with same parameters."
  (interactive)
  (cloudwatch-do-query))

(defun cloudwatch-increase-limit ()
  "Increase query limit and requery."
  (interactive)
  (setq cloudwatch-query-limit (* cloudwatch-query-limit 2))
  (message "Increasing limit to %d..." cloudwatch-query-limit)
  (cloudwatch-requery))

(defun cloudwatch-decrease-limit ()
  "Decrease query limit and requery."
  (interactive)
  (setq cloudwatch-query-limit (max 100 (/ cloudwatch-query-limit 2)))
  (message "Decreasing limit to %d..." cloudwatch-query-limit)
  (cloudwatch-requery))

(defun cloudwatch-do-tail ()
  "Execute the tail command with current parameters."
  (interactive)
  (let* ((buffer-name (cloudwatch--extract-buffer-name cloudwatch-current-log-group 'tail))
         (filter-args (if (and cloudwatch-current-filter
                               (not (string-empty-p cloudwatch-current-filter)))
                          (format " --filter-pattern %s"
                                  (shell-quote-argument cloudwatch-current-filter))
                        ""))
         (cmd (format "aws logs tail %s --region %s --since %dm --follow --format short%s"
                      (shell-quote-argument cloudwatch-current-log-group)
                      (shell-quote-argument (cloudwatch-get-region))
                      cloudwatch-current-minutes
                      filter-args))
         (process-environment (cons "AWS_PAGER="
                                    (cons "TERM=dumb"
                                          process-environment))))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((output-buffer (get-buffer-create buffer-name)))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert (format "Tailing: %s\nRegion: %s\nSince: %d minutes ago\n"
                        cloudwatch-current-log-group
                        (cloudwatch-get-region)
                        cloudwatch-current-minutes))
        (when (not (string-empty-p cloudwatch-current-filter))
          (insert (format "Filter: %s\n" cloudwatch-current-filter)))
        (insert "Keys: s=stop tailing  q=close buffer\n")
        (insert "─────────────────────────────────────────────────\n")
        (cloudwatch--setup-highlighting)
        (toggle-truncate-lines 1)
        (local-set-key (kbd "q") 'kill-current-buffer)
        (local-set-key (kbd "s") 'cloudwatch-stop-tail))
      (switch-to-buffer output-buffer)
      (let ((proc (start-process-shell-command
                   "cloudwatch-tail"
                   output-buffer
                   cmd)))
        (set-process-filter
         proc
         (lambda (process output)
           (when (buffer-live-p (process-buffer process))
             (with-current-buffer (process-buffer process)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert (ansi-color-apply output)))))))
        (set-process-sentinel
         proc
         (lambda (_process _event)
           ;; Silently handle process exit since we manage our own messaging
           nil))
        (message "Tailing logs... press 's' to stop, 'q' to quit.")))))

(defun cloudwatch-do-tail-safe ()
  "Execute tail command, returning to transient on validation errors."
  (interactive)
  (cloudwatch-with-transient-fallback
   (unless cloudwatch-current-log-group
     (user-error "Please select a log group first"))
   (cloudwatch-do-tail)))

;; Provide an intuitive way to stop the log stream without killing the buffer.
(defun cloudwatch-stop-tail ()
  "Stop the tail process in the current buffer without killing it."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (buf (current-buffer)))
    (if proc
        (progn
          (interrupt-process proc)
          ;; NOTE: If the stop message gets inserted immediately there may still be output in the process buffer that hasn't been flushed yet. A short delay lets the remaining output land first. The buf capture is necessary because by the time the timer fires, current-buffer could be anything. The buffer-live-p guard handles the case where the user kills the buffer in that half second.
          (run-with-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert "\n─────────────────────────────────────────────────\n"
                           "Tail stopped. Buffer preserved for inspection.\n"
                           "Press 'q' to close buffer.\n"))))))
          (message "Tail stopped. Buffer preserved for inspection."))
      (message "No active tail process in this buffer."))))

;;;; Insights query operations
(defun cloudwatch-set-insights-query ()
  "Set CloudWatch Insights query from presets or custom."
  (interactive)
  (cloudwatch-with-transient-fallback
   (let* ((choices (append
                    '(("Custom query" . custom))
                    cloudwatch-insights-presets))
          (choice (completing-read "Select Insights query: "
                                   (mapcar #'car choices)
                                   nil t nil 'cloudwatch-insights-history)))
     (setq cloudwatch-insights-query
           (if (string= choice "Custom query")
               (cloudwatch--read-multiline "Insights query"
                                           cloudwatch-insights-query)
             (cdr (assoc choice choices)))))
   (cloudwatch-transient)))

(defun cloudwatch-do-insights-query ()
  "Execute CloudWatch Insights query."
  (interactive)
  (let* ((buffer-name (format "*CW-Insights:%s:%s*"
                              (cloudwatch-get-region)
                              (car (last (split-string cloudwatch-current-log-group "/") 2))))
         (output-buffer (get-buffer-create buffer-name)))

    (message "Starting Insights query...")
    (let* ((start-time (cloudwatch--start-time-millis))
           (end-time (cloudwatch--end-time-millis))
           (start-cmd (format "aws logs start-query --log-group-name %s --region %s --start-time %d --end-time %d --query-string %s --output text"
                              (shell-quote-argument cloudwatch-current-log-group)
                              (shell-quote-argument (cloudwatch-get-region))
                              start-time
                              end-time
                              (shell-quote-argument cloudwatch-insights-query)))
           ;; Use our error-handling wrapper here
           (query-id-output (cloudwatch--run-aws-command start-cmd)))

      ;; Check if we got a valid query ID back
      (unless query-id-output
        (user-error "Failed to start Insights query"))

      (let ((query-id (string-trim query-id-output)))
        (when (string-empty-p query-id)
          (user-error "Failed to start Insights query - empty response"))

        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; Store query params for refresh
            (setq-local cloudwatch-insights-query-info
                        (list :log-group cloudwatch-current-log-group
                              :region cloudwatch-current-region
                              :minutes cloudwatch-current-minutes
                              :query cloudwatch-insights-query))
            (insert "═══ CloudWatch Insights Query ═══\n")
            (insert (format "Log Group: %s\n" cloudwatch-current-log-group))
            (insert (format "Region: %s\n" (cloudwatch-get-region)))
            (insert (format "Time Range: %s\n" (cloudwatch--time-description)))
            (insert (format "Query: %s\n" cloudwatch-insights-query))
            (insert "─────────────────────────────────\n")
            (insert "⏳ Query running...\n")))

        (switch-to-buffer output-buffer)

        ;; Poll for results
        (cloudwatch-insights-poll-results query-id output-buffer)))))

(defun cloudwatch-do-insights-query-safe ()
  "Execute Insights query, returning to transient on validation errors."
  (interactive)
  (cloudwatch-with-transient-fallback
   (unless cloudwatch-current-log-group
     (user-error "Please select a log group first"))

   (unless cloudwatch-insights-query
     (user-error "Please set an Insights query first"))
   (cloudwatch-do-insights-query)))

(defun cloudwatch-insights-poll-results (query-id buffer &optional start-time)
  "Poll for Insights QUERY-ID results in BUFFER.
START-TIME tracks when polling began for timeout detection."
  (let ((start-time (or start-time (float-time))))
    (run-with-timer
     2 nil
     (lambda ()
       (cond
        ;; Buffer killed, silently abandon it, no one cares
        ((not (buffer-live-p buffer))
         (message "Query buffer closed, abandoning poll"))

        ;; Timeout so we don't create an unbound timer chain and rip a hole in space-time
        ((> (- (float-time) start-time) cloudwatch-insights-poll-timeout)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (goto-char (point-min))
             (when (search-forward "⏳ Query running..." nil t)
               (replace-match (format "⏱ Timed out after %ds (query may still be running on AWS)"
                                      cloudwatch-insights-poll-timeout)))))
         (message "Insights query polling timed out"))

        ;; Polling normally with out a time paradox
        (t
         (condition-case err
             (let* ((cmd (format "aws logs get-query-results --query-id %s --region %s --output json"
                                 (shell-quote-argument query-id)
                                 (shell-quote-argument (cloudwatch-get-region))))
                    (output (cloudwatch--run-aws-command cmd))
                    (result (and output (json-parse-string output :object-type 'alist)))
                    (status (and result (alist-get 'status result))))
               (pcase status
                 ("Complete"
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (save-excursion
                        (goto-char (point-min))
                        (search-forward "⏳ Query running..." nil t)
                        (replace-match "✓ Query complete!"))
                      (goto-char (point-max))
                      (insert "\n")
                      (cloudwatch-insights-format-results (alist-get 'results result))
                      (read-only-mode 1)
                      (cloudwatch-insights-results-mode 1)
                      (cloudwatch--setup-highlighting)
                      (message "Insights query complete!"))))

                 ("Failed"
                  (with-current-buffer buffer
                    (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (when (search-forward "⏳ Query running..." nil t)
                        (replace-match "✗ Query failed!")))
                    (message "Insights query failed!")))

                 ((or "Running" "Scheduled" `nil)
                  (cloudwatch-insights-poll-results query-id buffer start-time))))
           (error
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (when (search-forward "⏳ Query running..." nil t)
                  (replace-match (format "Error: %s" (error-message-string err))))))
            (message "Insights query error: %s" (error-message-string err))))))))))

(defun cloudwatch-insights-format-results (results)
  "Format Insights RESULTS for display.
Uses buffer-local `cloudwatch-insights-query-info' for metadata."
  (if (or (not results)
          (seq-empty-p results))
      (insert (propertize "No results found.\n" 'face 'font-lock-comment-face))
    ;; Store results for detail view
    (setq-local cloudwatch-insights-results results)

    ;; Get field names from first result
    (let ((fields (mapcar (lambda (item) (alist-get 'field item)) (aref results 0))))
      ;; Get column widths dynamically
      (let ((field-widths (mapcar #'cloudwatch--get-column-width fields)))
        ;; Print header
        (insert (mapconcat (lambda (pair)
                             (let ((field (car pair))
                                   (width (cdr pair)))
                               (format (format "%%-%ds" width)
                                       (truncate-string-to-width field width nil nil "…"))))
                           (cl-mapcar #'cons fields field-widths) " | "))
        (insert "\n")
        (insert (make-string (+ (apply #'+ field-widths)
                                (* (1- (length fields)) 3)) ?─))
        (insert "\n")
        ;; Print each row with click handler
        (dotimes (i (length results))
          (let ((row (aref results i))
                (start (point)))
            (insert (mapconcat (lambda (pair)
                                 (let* ((item (car pair))
                                        (width (cdr pair))
                                        (value (alist-get 'value item)))
                                   (format (format "%%-%ds" width)
                                           (truncate-string-to-width
                                            (or value "")
                                            (1- width) nil nil "…"))))
                               (cl-mapcar #'cons row field-widths) " | "))
            (insert "\n")
            ;; Add properties to make the row clickable
            (add-text-properties start (point)
                                 (list 'cloudwatch-row-index i
                                       'keymap (let ((map (make-sparse-keymap)))
                                                 (define-key map (kbd "RET") 'cloudwatch-insights-show-detail)
                                                 map)
                                       'help-echo "Press RET to view full record")))))))

  (insert "\n" (propertize "Keys: RET=details  g=refresh  +=expand time  q=quit"
                           'face 'font-lock-comment-face)))

(defun cloudwatch-insights-show-detail ()
  "Show full details for the log entry at point."
  (interactive)
  (let ((row-index (get-text-property (point) 'cloudwatch-row-index)))
    (when row-index
      (let* ((results cloudwatch-insights-results)
             (row (aref results row-index)))
        (with-current-buffer (get-buffer-create "*CloudWatch Entry Detail*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "═══ CloudWatch Log Entry Detail ═══\n\n" 'face 'bold))

            ;; Show all fields with full content
            (mapc (lambda (item)
                    (let ((field (alist-get 'field item))
                          (value (alist-get 'value item)))
                      (insert (propertize (format "%s:\n" field)
                                          'face 'font-lock-keyword-face))
                      ;; Pretty-print JSON if detected because no one likes ugly JSON
                      (if (and value
                               (string-match "^[[:space:]]*[{\\[]" value))
                          (condition-case nil
                              (progn
                                (insert value)
                                (save-excursion
                                  (backward-char (length value))
                                  (json-pretty-print (point) (point-max))))
                            (error (insert value)))
                        (insert (or value "null")))
                      (insert "\n\n")))
                  row)

            (goto-char (point-min))
            (cloudwatch-detail-mode))
          (pop-to-buffer (current-buffer)))))))

(defun cloudwatch-rerun-insights ()
  "Rerun the Insights query with stored parameters."
  (interactive)
  (if cloudwatch-insights-query-info
      (let ((cloudwatch-current-log-group (plist-get cloudwatch-insights-query-info :log-group))
            (cloudwatch-current-region (plist-get cloudwatch-insights-query-info :region))
            (cloudwatch-current-minutes (plist-get cloudwatch-insights-query-info :minutes))
            (cloudwatch-insights-query (plist-get cloudwatch-insights-query-info :query)))
        (cloudwatch-do-insights-query))
    ;; Fallback to current globals
    (cloudwatch-do-insights-query)))

(defun cloudwatch-insights-expand-time ()
  "Double the time range and rerun Insights query.
Only available in relative time mode."
  (interactive)
  (cond
   ((null cloudwatch-insights-query-info)
    (message "No query info stored - run a query first"))
   ((eq cloudwatch-time-mode 'absolute)
    (message "Time expansion not available in date range mode - use T to adjust"))
   (t
    (let ((new-minutes (* 2 (plist-get cloudwatch-insights-query-info :minutes))))
      (setq cloudwatch-insights-query-info
            (plist-put cloudwatch-insights-query-info :minutes new-minutes))
      (message "Expanding to %d minutes..." new-minutes)
      (cloudwatch-rerun-insights)))))

(define-derived-mode cloudwatch-detail-mode special-mode "CW-Detail"
  "Mode for viewing CloudWatch log entry details."
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (when (boundp 'cloudwatch-insights-show-detail)
                  (call-interactively 'cloudwatch-insights-show-detail)))))

;;;; Transient magic
(transient-define-prefix cloudwatch-transient ()
  "CloudWatch Logs Viewer"
  :value '()
  ["CloudWatch Settings"
   [("r" "Region" cloudwatch-set-region
     :description (lambda () (format "Region: %s" (cloudwatch-get-region))))
    ("l" "Log Group" cloudwatch-set-log-group
     :description (lambda () (format "Log Group: %s"
                                     (or (and cloudwatch-current-log-group
                                              (truncate-string-to-width cloudwatch-current-log-group 50))
                                         "Not set"))))
    ("L" "Browse Log Groups" cloudwatch-browse-log-groups)]
   [("m" "Minutes back" cloudwatch-set-minutes
     :description (lambda () (format "Minutes: %d" cloudwatch-current-minutes)))
    ("M" "Query limit" cloudwatch-set-query-limit
     :description (lambda () (format "Query limit: %d events" cloudwatch-query-limit)))
    ("f" "Filter pattern" cloudwatch-set-filter
     :description (lambda () (format "Filter: %s"
                                     (if (string-empty-p cloudwatch-current-filter)
                                         "None"
                                       (truncate-string-to-width cloudwatch-current-filter 40)))))
    ("w" "Wide mode" cloudwatch-toggle-wide-mode
     :description (lambda () (format "Wide mode: %s" (if cloudwatch-wide-mode "ON" "OFF"))))
    ("R" "Refresh cache" cloudwatch-refresh-cache)
    ("T" "Time range" cloudwatch-set-time-range
     :description (lambda ()
                    (if (eq cloudwatch-time-mode 'absolute)
                        (format "Range: %s → %s"
                                (format-time-string "%m-%d %H:%M" cloudwatch-start-time)
                                (format-time-string "%m-%d %H:%M" cloudwatch-end-time))
                      "Range: relative (use Minutes)")))
    ("C-t" "Clear time range" cloudwatch-clear-time-range
     :if (lambda () (eq cloudwatch-time-mode 'absolute)))]]
  ["Quick Filters"
   :description "Simple pattern matching for live tailing and quick searches"
   [("E" "Errors only" (lambda () (interactive) (setq cloudwatch-current-filter "ERROR") (cloudwatch-transient)))
    ("W" "Warnings" (lambda () (interactive) (setq cloudwatch-current-filter "WARN") (cloudwatch-transient)))
    ("x" "5xx errors" (lambda () (interactive) (setq cloudwatch-current-filter "{ $.statusCode >= 500 }") (cloudwatch-transient)))]
   [("n" "Namespace" cloudwatch-set-namespace-filter)
    ("p" "Pod name" cloudwatch-set-pod-filter)
    ("c" "Clear filter" (lambda () (interactive) (setq cloudwatch-current-filter "") (cloudwatch-transient)))]]
  ["CloudWatch Insights"
   :description "Advanced queries for aggregations, stats, and complex analysis"
   [("i" "Set Insights query" cloudwatch-set-insights-query
     :description (lambda ()
                    (if cloudwatch-insights-query
                        (format "Query: %s" (truncate-string-to-width cloudwatch-insights-query 40))
                      "Query: Not set")))
    ("I" "Run Insights query" cloudwatch-do-insights-query-safe :transient nil)]]
  ["Favorites"
   :class transient-column
   :setup-children cloudwatch-favorites-setup]
  ["Favorites Management"
   ("a" "Add current to favorites"
    (lambda () (interactive)
      (if cloudwatch-current-log-group
          (progn
            (unless (member cloudwatch-current-log-group cloudwatch-favorite-log-groups)
              (push cloudwatch-current-log-group cloudwatch-favorite-log-groups))
            (message "Added to session favorites: %s"
                     (truncate-string-to-width cloudwatch-current-log-group 50))
            (cloudwatch-transient))
        (message "No log group selected"))))
   ("d" "Remove from favorites" cloudwatch-remove-from-favorites)]
  ["Actions"
   [("t" "Tail: Live streaming logs with filters" cloudwatch-do-tail-safe :transient nil)
    ("Q" "Query: Snapshot search with filters" cloudwatch-do-query-safe :transient nil)
    ("q" "Quit" transient-quit-one)]])

(defun cloudwatch-favorites-setup (_)
  "Dynamically generate favorite buttons."
  (if cloudwatch-favorite-log-groups
      (transient-parse-suffixes
       'cloudwatch-transient
       (cl-loop for log-group in (seq-take cloudwatch-favorite-log-groups 5)
                for i from 1
                collect (list (format "%d" i)
                              (truncate-string-to-width log-group 60)
                              `(lambda () (interactive)
                                 (setq cloudwatch-current-log-group ,log-group)
                                 (cloudwatch-transient)))))
    ;; Return a properly parsed suffix for empty state
    (transient-parse-suffixes
     'cloudwatch-transient
     '(("!" "No favorites yet - press 'a' to add" ignore)))))

;;;###autoload
(defun cloudwatch ()
  "Open CloudWatch logs viewer."
  (interactive)
  (unless (executable-find "aws")
    (user-error "AWS CLI not found. Please install aws-cli"))
  (cloudwatch-transient))

(provide 'cloudwatch)
;;; cloudwatch.el ends here

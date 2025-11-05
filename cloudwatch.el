;;; cloudwatch.el --- AWS CloudWatch log viewer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 @rand-fu
;;
;; Author: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Maintainer: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Created: November 04, 2025
;; Modified: November 04, 2025
;; Version: 0.1.1
;; Keywords: tools aws cloud logs
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
;; This package provides an interface for viewing AWS CloudWatch logs
;; directly from Emacs, eliminating the need to use the AWS web console.
;;
;; Features:
;; - Interactive transient-based interface for easy navigation
;; - Live log tailing with automatic updates
;; - Snapshot queries for historical log analysis
;; - Advanced filter patterns with quick presets for common searches
;; - Region switching for multi-region deployments
;; - Favorites management for frequently accessed log groups
;; - Syntax highlighting for log levels (ERROR, WARN, INFO, DEBUG)
;; - Asynchronous operations to keep Emacs responsive
;;
;; Usage:
;;   M-x cloudwatch RET
;;
;; This will open a transient menu where you can:
;; - Select AWS region and log groups
;; - Set time ranges and filters
;; - Choose between live tailing or snapshot queries
;; - Access favorite log groups with quick keys
;;
;; The package requires AWS CLI to be installed and configured with
;; appropriate credentials. It works particularly well with EKS/Container
;; Insights logs but supports any CloudWatch log group.
;;
;; See README.md for detailed configuration and filter pattern examples.
;;
;;  Description:
;;  An Emacs interface for AWS CloudWatch logs with live tailing, querying,
;;  and filtering capabilities through a transient-based UI.
;;
;;; Code:

(require 'transient)
(require 'ansi-color)

(defgroup cloudwatch nil
  "AWS CloudWatch log viewer."
  :group 'tools)

(defcustom cloudwatch-aws-region "us-west-2"
  "AWS region for CloudWatch."
  :type 'string
  :group 'cloudwatch)

(defcustom cloudwatch-default-region "us-west-2"
  "Default AWS region for CloudWatch logs."
  :type 'string
  :group 'cloudwatch)

(defvar cloudwatch-current-log-group nil)
(defvar cloudwatch-current-minutes 5)
(defvar cloudwatch-current-filter "")
(defvar cloudwatch-current-region cloudwatch-default-region)
(defvar cloudwatch-log-groups-cache nil)
(defvar cloudwatch-cache-time nil)

(defcustom cloudwatch-favorite-log-groups nil
  "List of frequently used CloudWatch log groups.
Example: \='(\"/aws/containerinsights/prod/application\"
           \"/aws/lambda/my-function\")"
  :type '(repeat string)
  :group 'cloudwatch)

(defun cloudwatch-check-aws-cli ()
  "Check if AWS CLI is installed and configured."
  (unless (executable-find "aws")
    (user-error "AWS CLI not found. Please install aws-cli"))
  (when (string-match "Unable to locate credentials"
                      (shell-command-to-string "aws sts get-caller-identity 2>&1"))
    (user-error "AWS credentials not configured")))

(defun cloudwatch-extract-buffer-name (log-group &optional mode)
  "Extract meaningful buffer name from LOG-GROUP path and optional MODE."
  (let ((prefix (if (eq mode 'query) "CW-Query" "CW")))
    (cond
     ((string-match "/aws/containerinsights/\\([^/]+\\)/\\([^/]+\\)" log-group)
      (format "*%s:%s:%s-%s*"
              prefix
              cloudwatch-current-region
              (match-string 1 log-group)
              (match-string 2 log-group)))
     ((string-match "/aws/lambda/\\([^/]+\\)" log-group)
      (format "*%s:%s:lambda-%s*"
              prefix
              cloudwatch-current-region
              (match-string 1 log-group)))
     (t (let ((parts (split-string log-group "/")))
          (format "*%s:%s:%s*"
                  prefix
                  cloudwatch-current-region
                  (string-join (last parts 2) "-")))))))

(defun cloudwatch-list-log-groups (&optional refresh)
  "List all log groups in current region. Use cache unless REFRESH is true."
  (when (or refresh
            (null cloudwatch-log-groups-cache)
            (null cloudwatch-cache-time)
            (> (- (float-time) cloudwatch-cache-time) 300)) ; 5 min cache
    (message "Fetching log groups from %s..." cloudwatch-current-region)
    (let* ((cmd (format "aws logs describe-log-groups --region %s --query 'logGroups[].logGroupName' --output text"
                        cloudwatch-current-region))
           (output (shell-command-to-string cmd)))
      (setq cloudwatch-log-groups-cache
            (split-string output "\t\\|\n" t))
      (setq cloudwatch-cache-time (float-time))
      (message "Found %d log groups" (length cloudwatch-log-groups-cache))))
  cloudwatch-log-groups-cache)

(defcustom cloudwatch-query-limit 2500
  "Maximum number of log events to retrieve in query mode."
  :type 'integer
  :group 'cloudwatch)

(defun cloudwatch-do-query ()
  "Query logs from the past N minutes without tailing."
  (interactive)
  (unless cloudwatch-current-log-group
    (user-error "Please select a log group first"))
  (let* ((buffer-name (cloudwatch-extract-buffer-name cloudwatch-current-log-group 'query))
         (filter-args (if (and cloudwatch-current-filter
                               (not (string-empty-p cloudwatch-current-filter)))
                          (format " --filter-pattern '%s'" cloudwatch-current-filter)
                        ""))
         (cmd (format "aws logs filter-log-events --log-group-name '%s' --region %s --start-time %d --limit %d --output text%s"
                      cloudwatch-current-log-group
                      cloudwatch-current-region
                      (* (- (truncate (float-time)) (* cloudwatch-current-minutes 60)) 1000)
                      cloudwatch-query-limit
                      filter-args))
         ;; Set environment to avoid terminal warnings
         (process-environment (cons "AWS_PAGER="
                                    (cons "TERM=dumb"
                                          process-environment))))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((output-buffer (get-buffer-create buffer-name)))
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert (format "Querying: %s\nRegion: %s\nTime range: Last %d minutes\nLimit: %d events\n"
                        cloudwatch-current-log-group
                        cloudwatch-current-region
                        cloudwatch-current-minutes
                        cloudwatch-query-limit))  ; Show limit in header
        (when (not (string-empty-p cloudwatch-current-filter))
          (insert (format "Filter: %s\n" cloudwatch-current-filter)))
        (insert "─────────────────────────────────────────────────\n")
        (insert "⏳ Loading logs...\n")
        (cloudwatch-setup-highlighting)
        (toggle-truncate-lines 1)
        (local-set-key (kbd "q") 'kill-current-buffer)
        (local-set-key (kbd "g") 'cloudwatch-requery)
        (local-set-key (kbd "+" ) 'cloudwatch-increase-limit)
        (local-set-key (kbd "-" ) 'cloudwatch-decrease-limit))
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
               ;; Clean up output
               (save-excursion
                 (goto-char (point-min))
                 (when (search-forward "⏳ Loading logs..." nil t)
                   (replace-match (format "✓ Query complete (max %d events)" cloudwatch-query-limit)))
                 ;; Remove EVENTS prefix from each line
                 (goto-char (point-min))
                 (while (re-search-forward "^EVENTS\t" nil t)
                   (replace-match ""))
                 ;; Check if we hit the limit
                 (goto-char (point-max))
                 (let ((line-count (count-lines (point-min) (point-max))))
                   (when (>= line-count (+ cloudwatch-query-limit 5)) ; +5 for header lines
                     (goto-char (point-max))
                     (insert "\n⚠️  Result limit reached. Press '+' to increase limit and requery.")))
                 (goto-char (point-min)))
               (read-only-mode 1)
               (message "Query complete. Press 'g' to refresh, '+/-' to adjust limit, 'q' to quit.")))))
        (message "Querying logs asynchronously (limit: %d)..." cloudwatch-query-limit)))))

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
  (unless cloudwatch-current-log-group
    (user-error "Please select a log group first"))
  (let* ((buffer-name (cloudwatch-extract-buffer-name cloudwatch-current-log-group 'tail))
         (filter-args (if (and cloudwatch-current-filter
                               (not (string-empty-p cloudwatch-current-filter)))
                          (format " --filter-pattern '%s'" cloudwatch-current-filter)
                        ""))
         (cmd (format "aws logs tail '%s' --region %s --since %dm --follow --format short%s"
                      cloudwatch-current-log-group
                      cloudwatch-current-region
                      cloudwatch-current-minutes
                      filter-args))
         ;; Set environment to avoid terminal warnings
         (process-environment (cons "AWS_PAGER="
                                    (cons "TERM=dumb"
                                          process-environment))))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (async-shell-command cmd buffer-name)
    (with-current-buffer buffer-name
      (ansi-color-for-comint-mode-on)
      (font-lock-mode 1)
      (cloudwatch-setup-highlighting)
      (toggle-truncate-lines 1)
      (goto-char (point-max))
      (local-set-key (kbd "q") 'kill-current-buffer))))

(defun cloudwatch-requery ()
  "Rerun the query with same parameters."
  (interactive)
  (cloudwatch-do-query))

(defun cloudwatch-setup-highlighting ()
  "Setup common highlighting patterns."
  (font-lock-mode 1)
  (highlight-regexp "ERROR\\|FATAL\\|Exception" 'hi-red-b)
  (highlight-regexp "WARN\\|WARNING" 'hi-yellow)
  (highlight-regexp "INFO" 'hi-green)
  (highlight-regexp "DEBUG" 'hi-blue)
  (highlight-regexp "\"[^\"]+\":" 'font-lock-keyword-face)
  (highlight-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" 'font-lock-comment-face))

(transient-define-prefix cloudwatch-transient ()
  "CloudWatch Logs Viewer"
  :value '()
  ["CloudWatch Settings"
   [("r" "Region" cloudwatch-set-region
     :description (lambda () (format "Region: %s" cloudwatch-current-region)))
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
    ("R" "Refresh cache" cloudwatch-refresh-cache)]]
  ["Quick Filters"
   [("E" "Errors only" (lambda () (interactive) (setq cloudwatch-current-filter "ERROR") (cloudwatch-transient)))
    ("W" "Warnings" (lambda () (interactive) (setq cloudwatch-current-filter "WARN") (cloudwatch-transient)))
    ("5" "5xx errors" (lambda () (interactive) (setq cloudwatch-current-filter "{ $.statusCode >= 500 }") (cloudwatch-transient)))]
   [("n" "Namespace" cloudwatch-set-namespace-filter)
    ("p" "Pod name" cloudwatch-set-pod-filter)
    ("c" "Clear filter" (lambda () (interactive) (setq cloudwatch-current-filter "") (cloudwatch-transient)))]]
  ["Favorites"
   :class transient-column
   :setup-children cloudwatch-favorites-setup]
  ["Actions"
   [("t" "Start Tailing (live)" cloudwatch-do-tail :transient nil)
    ("Q" "Query logs (snapshot)" cloudwatch-do-query :transient nil)
    ("q" "Quit" transient-quit-one)]])

(defun cloudwatch-set-query-limit ()
  "Set the query result limit."
  (interactive)
  (setq cloudwatch-query-limit
        (read-number "Maximum events to retrieve: " cloudwatch-query-limit))
  (cloudwatch-transient))

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
    ;; Show helpful message when no favorites
    (list (list "!" "No favorites yet" 'ignore))))

(defun cloudwatch-set-region ()
  "Set AWS region."
  (interactive)
  (setq cloudwatch-current-region
        (completing-read "AWS Region: "
                         '("us-west-1" "us-west-2" "us-east-1" "us-east-2"
                           "eu-west-1" "eu-central-1" "ap-southeast-1" "ap-northeast-1")
                         nil nil cloudwatch-current-region))
  ;; Clear cache when region changes
  (setq cloudwatch-log-groups-cache nil)
  (cloudwatch-transient))

(defun cloudwatch-set-log-group ()
  "Set the log group from favorites or type custom."
  (interactive)
  (setq cloudwatch-current-log-group
        (completing-read "Log group (favorites + custom): "
                         cloudwatch-favorite-log-groups
                         nil nil nil 'cloudwatch-history))
  (cloudwatch-transient))

(defun cloudwatch-browse-log-groups ()
  "Browse and select from all log groups in region."
  (interactive)
  (let ((log-groups (cloudwatch-list-log-groups)))
    (setq cloudwatch-current-log-group
          (completing-read (format "Select log group (%d available): " (length log-groups))
                           log-groups
                           nil t nil 'cloudwatch-history))
    ;; Offer to add to favorites
    (when (and cloudwatch-current-log-group
               (not (member cloudwatch-current-log-group cloudwatch-favorite-log-groups))
               (y-or-n-p "Add to favorites? "))
      (customize-save-variable 'cloudwatch-favorite-log-groups
                               (append cloudwatch-favorite-log-groups
                                       (list cloudwatch-current-log-group)))))
  (cloudwatch-transient))

(defun cloudwatch-refresh-cache ()
  "Refresh the log groups cache."
  (interactive)
  (cloudwatch-list-log-groups t)
  (message "Cache refreshed!")
  (cloudwatch-transient))

(defun cloudwatch-set-minutes ()
  "Set minutes to look back."
  (interactive)
  (setq cloudwatch-current-minutes
        (read-number "Minutes back: " cloudwatch-current-minutes))
  (cloudwatch-transient))

(defun cloudwatch-set-filter ()
  "Set filter pattern."
  (interactive)
  (setq cloudwatch-current-filter
        (read-string "Filter pattern: " cloudwatch-current-filter))
  (cloudwatch-transient))

(defun cloudwatch-set-namespace-filter ()
  "Set namespace filter."
  (interactive)
  (let ((namespace (read-string "Namespace: ")))
    (setq cloudwatch-current-filter
          (format "{ $.kubernetes.namespace_name = \"%s\" }" namespace)))
  (cloudwatch-transient))

(defun cloudwatch-set-pod-filter ()
  "Set pod name filter."
  (interactive)
  (let ((pod (read-string "Pod name (supports * wildcards): ")))
    (setq cloudwatch-current-filter
          (format "{ $.kubernetes.pod_name = *%s* }" pod)))
  (cloudwatch-transient))

;;;###autoload
(defun cloudwatch ()
  "Open CloudWatch logs viewer."
  (interactive)
  (cloudwatch-transient))

(provide 'cloudwatch)
;;; cloudwatch.el ends here

;;; cloudwatch-doom.el --- Doom integration for cloudwatch -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "27.1") (cloudwatch "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;; 
;; Licensed under the GNU General Public License v3.0 or later.
;; See the LICENSE file in the project root for full license text.
;;
;;; Commentary:
;;
;; Doom Emacs integration for the cloudwatch package.
;;
;; This module provides:
;; - Doom-style keybinding under SPC o C for quick access
;; - Automatic AWS region detection from environment variables
;; - Integration with Doom's package management system
;;
;; Installation:
;;   Add to ~/.doom.d/packages.el:
;;     (package! cloudwatch :recipe (:host github :repo "rand-fu/cloudwatch-el"))
;;
;;   Add to ~/.doom.d/config.el:
;;     (use-package! cloudwatch
;;       :config
;;       (load! "cloudwatch-doom" (file-name-directory (locate-library "cloudwatch"))))
;;
;; The keybinding SPC o C will open the CloudWatch transient interface,
;; giving you quick access to your AWS logs without leaving Emacs.
;;
;;  Description:
;;  Doom Emacs keybindings and configuration for the cloudwatch package.
;;
;;; Code:

(require 'cloudwatch)

;; Doom-specific keybindings
(map! :leader
      (:prefix ("o" . "open")
       :desc "CloudWatch" "C" #'cloudwatch))

;; Doom-specific configuration
(after! cloudwatch
  (setq cloudwatch-aws-region (or (getenv "AWS_REGION") "us-east-1")))

(provide 'cloudwatch-doom)
;;; cloudwatch-doom.el ends here

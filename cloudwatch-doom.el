;;; cloudwatch-doom.el --- Doom Emacs integration for cloudwatch.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 @rand-fu
;;
;; Author: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Maintainer: Randol Reeves <randol.reeves+emacs@gmail.com>
;; Created: November 04, 2025
;; Modified: November 18, 2026
;; Version: 1.1.0
;; Keywords: tools aws cloudwatch logs monitoring devops kubernetes observability
;; Homepage: https://github.com/rand-fu/cloudwatch-el
;; Package-Requires: ((emacs "29.1") (cloudwatch "1.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; Licensed under the GNU General Public License v3.0 or later.
;; See the LICENSE file in the project root for full license text.
;;
;;; Commentary:
;;
;; Doom Emacs keybindings for cloudwatch.el.
;; Binds SPC o C to open the CloudWatch transient interface.
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
;;; Code:

(require 'cloudwatch)

;; This DOOM commit https://github.com/doomemacs/core/commit/635bc939d3cb835873d7039a6505d78cdb99d313
;; introduces a breaking change that causes the below to break using the "o" prefix.
;; Rework it to fit the new pattern
(map! :leader
      (:prefix "o"
       :desc "CloudWatch" "C" #'cloudwatch))

(provide 'cloudwatch-doom)
;;; cloudwatch-doom.el ends here

;;; frame-workflow-magit.el --- Utilties that depend on magit -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (magit "2.0"))
;; Keywords: vc
;; URL: https://github.com/akirak/frame-workflow

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides extra utilities with magit dependency.

;;; Code:

(require 'magit)

;;;###autoload
(defun frame-workflow-magit-same-window ()
  "Run `magit-status' in the same window.

This function is intended as the value for
`frame-workflow-directory-frame-action'."
  (let ((magit-display-buffer-function
         #'magit-display-buffer-same-window-except-diff-v1))
    (call-interactively #'magit-status)))

;;;###autoload
(defun frame-workflow-magit-only-window ()
  "Make `magit-status' the only window in the frame."
  (frame-workflow-magit-same-window)
  (delete-other-windows))

(provide 'frame-workflow-magit)
;;; frame-workflow-magit.el ends here

;;; frame-workflow-purpose.el --- Integrate frame-workflow with frame-purpose -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (frame-purpose "1.0"))
;; Keywords: frames
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

;; This library provides frame-workflow integration for frame-purpose.el.

;;; Code:

(require 'frame-purpose)
(require 'frame-workflow)

(defun frame-workflow-purpose-init-function ()
  "Apply a workflow prototype based on the frame parameter.

To integrate frame-workflow with frame-purpose, set
`frame-purpose--initial-buffer-fn' to this function. When you create a frame
using frame-purpose, you can apply a workflow by passing its prototype name to
`frame-purpose-make-frame' as `workflow-prototype' parameter. The name should be
a symbol."
  (when-let ((prototype (frame-parameter nil 'workflow-prototype)))
    (frame-workflow-apply-prototype prototype t)))

(defun frame-workflow-purpose-setup ()
  "Set up frame-purpose for frame-workflow.

This function sets `frame-purpose--initial-buffer-fn' to a function provided by
frame-workflow package so that you can apply a workflow prototype to a
purpose-specific frame by passing `workflow-prototype' frame parameter.

To use the feature, you also have to turn on `frame-purpose-mode'."
  (setq frame-purpose--initial-buffer-fn 'frame-workflow-purpose-init-function))

(provide 'frame-workflow-purpose)
;;; frame-workflow-purpose.el ends here
